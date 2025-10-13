module Client where

-- Gloss & System
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

-- Network & Concurrency
import Network.Socket
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Control.Concurrent.MVar

-- Game Modules
import GameMes
import GameData
import GameRender

-- Trạng thái tổng thể của Client, được 'gloss' quản lý
data ClientState = ClientState
    { gameState     :: GameState
    , networkSocket :: Socket
    , myPlayerID    :: PlayerID
    , gameSprites   :: GameSprites
    }

main :: IO ()
main = withSocketsDo $ do
    -- 1. TẢI HÌNH ẢNH
    putStrLn "Loading sprites..."
    playerPic <- loadJuicyPNG_ "assets/spaceship.png"
    enemyPic  <- loadJuicyPNG_ "assets/enemy.png"
    bulletPic <- loadJuicyPNG_ "assets/bullet.png"
    itemPic   <- loadJuicyPNG_ "assets/item.png"
    let sprites = GameSprites playerPic enemyPic bulletPic itemPic
    putStrLn "All sprites loaded."

    -- 2. THIẾT LẬP KẾT NỐI MẠNG
    putStrLn "Connecting to server..."
    let serverIP = "127.0.0.1"
    let serverPort = "8080"
    addrInfos <- getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just serverIP) (Just serverPort)
    sock <- socket (addrFamily $ head addrInfos) Datagram defaultProtocol
    connect sock (addrAddress $ head addrInfos)
    putStrLn "Network socket connected."

    -- 3. KHỞI TẠO MVAR VÀ LUỒNG MẠNG
    let initialGameState = GameState [] [] [] [] 0 0 0 0 0 False
    gameStateMVar <- newMVar initialGameState
    _ <- forkIO $ receiverLoop sock gameStateMVar

    -- 4. GỬI YÊU CẦU THAM GIA GAME
    let myPlayerID = Player1
    let joinMsg = JoinGame myPlayerID
    _ <- send sock (LBS.toStrict $ encode joinMsg)
    
    -- 5. KHỞI CHẠY VÒNG LẶP GAME CỦA GLOSS
    let displayMode = InWindow "Haskell Shooter" (800, 600) (100, 100)
    let initialState = ClientState initialGameState sock myPlayerID sprites    -- Sử dụng 'sock' đã được kết nối ở trên

    putStrLn "Starting game loop..."
    play
        displayMode
        black
        60
        initialState
        drawHandler
        inputHandler
        (updateHandler gameStateMVar)

-- Hàm load ảnh phụ trợ để xử lý lỗi
loadJuicyPNG_ :: FilePath -> IO Picture
loadJuicyPNG_ path = loadJuicyPNG path >>= maybe (fail $ "Failed to load: " ++ path) return

----------------------------------------------------
-- CÁC HÀM CỦA GLOSS - GỌN GÀNG HƠN
----------------------------------------------------

-- 1. HÀM VẼ
drawHandler :: ClientState -> Picture
drawHandler state = render (gameSprites state) (gameState state)

-- 2. HÀM INPUT
inputHandler :: Event -> ClientState -> ClientState
inputHandler event state =
    let sock = networkSocket state
        pId  = myPlayerID state
    in case event of
        EventKey (Char 'q') Down _ _ -> unsafePerformIO exitSuccess  -- Bấm "q" để thoát
 
        -- Các phím di chuyển
        EventKey (Char 'w') Down _ _ -> sendAction sock pId MoveUp state
        EventKey (Char 's') Down _ _ -> sendAction sock pId MoveDown state
        EventKey (Char 'a') Down _ _ -> sendAction sock pId MoveLeft state
        EventKey (Char 'd') Down _ _ -> sendAction sock pId MoveRight state

        -- Bấm "Space" để bắn
        EventKey (SpecialKey KeySpace) Down _ _ -> sendAction sock pId Shoot state
        _ -> state

-- 3. HÀM UPDATE
updateHandler :: MVar GameState -> Float -> ClientState -> ClientState
updateHandler mvar _ currentState = unsafePerformIO $ do
    tryReadMVar mvar >>= \maybeNewState ->
        return $ case maybeNewState of
            Just newGameState -> currentState { gameState = newGameState }
            Nothing           -> currentState

-- CÁC HÀM PHỤ TRỢ KHÁC VÀ LUỒNG MẠNG
sendAction :: Socket -> PlayerID -> Action -> ClientState -> ClientState
sendAction sock pId action state = unsafePerformIO $ do
    let msg = PlayerAction pId action
    void $ send sock (LBS.toStrict $ encode msg)
    return state

receiverLoop :: Socket -> MVar GameState -> IO ()
receiverLoop sock gameStateMVar = forever $ do
    byteString <- recv sock 1024 -- <<< SỬA LẠI CHO ĐÚNG
    let serverMsg = decode (LBS.fromStrict byteString) :: ServerMessage
    case serverMsg of
        Welcome _ gs -> void $ swapMVar gameStateMVar gs
        UpdateGame gs -> void $ swapMVar gameStateMVar gs
        _ -> return ()