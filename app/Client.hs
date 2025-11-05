module Main where

-- Gloss & System
import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs) -- <-- THÊM DÒNG NÀY

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
data UiPhase = InMenu | InGame

data ClientState = ClientState
    { gameState     :: GameState
    , networkSocket :: Socket
    , myPlayerID    :: PlayerID
    , gameSprites   :: GameSprites
    , windowSize    :: (Int, Int)
    , uiPhase       :: UiPhase
    , selMode       :: GameMode
    , selPlayer     :: PlayerID
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
    let ai = head addrInfos
    sock <- socket (addrFamily ai) Datagram defaultProtocol
    connect sock (addrAddress ai)
    putStrLn "Network socket connected."

    -- 3. KHỞI TẠO MVAR VÀ LUỒNG MẠNG
    --                Player Enemy Item Level Left Spwn Wins Loss ItemSpawn EnemySpawn Bullets Shoot Mode
    let initialGameState = GameState [] [] [] 0 0 0 0 0 5.0 2.0 [] False Coop
    
    gameStateMVar <- newMVar initialGameState
    _ <- forkIO $ receiverLoop sock gameStateMVar

    -- 4. XÁC ĐỊNH PLAYERID TỪ THAM SỐ DÒNG LỆNH (SỬA LOGIC NẶNG)
    args <- getArgs
    let pID = case args of
                ("player2":_) -> Player2
                _             -> Player1 -- Mặc định là Player1
    
    putStrLn $ "Ready. Opened Menu. Default selection: " ++ show pID
    
    -- 6. KHỞI CHẠY VÒNG LẶP GAME CỦA GLOSS (TOÀN MÀN HÌNH)
    let displayMode = FullScreen
    let initialState = ClientState initialGameState sock pID sprites (1920,1080) InMenu Coop pID

    let customBackground = makeColorI 25 25 112 255
    putStrLn "Starting game loop..."
    play
        displayMode
        customBackground
        60
        initialState
        drawHandler
        inputHandler
        (updateHandler gameStateMVar)

-- Hàm load ảnh phụ trợ để xử lý lỗi
loadJuicyPNG_ :: FilePath -> IO Picture
loadJuicyPNG_ path = do
    maybePic <- loadJuicyPNG path
    case maybePic of
        Just pic -> return pic
        Nothing  -> do
            putStrLn $ "Cannot load image: " ++ path
            return blank

----------------------------------------------------
-- CÁC HÀM CỦA GLOSS
----------------------------------------------------

-- 1. HÀM VẼ
drawHandler :: ClientState -> Picture
drawHandler state = case uiPhase state of
    InMenu -> renderMenu (selMode state) (selPlayer state)
    InGame -> render (gameSprites state) (gameState state) (windowSize state)

-- 2. HÀM INPUT
inputHandler :: Event -> ClientState -> ClientState
inputHandler event state =
    let sock = networkSocket state
        pId  = myPlayerID state
    in case uiPhase state of
        InMenu -> case event of
            EventKey (Char 'q') Down _ _ -> unsafePerformIO exitSuccess
            EventKey (SpecialKey KeyLeft)  Down _ _ -> state { selMode = Coop }
            EventKey (SpecialKey KeyRight) Down _ _ -> state { selMode = Solo }
            EventKey (SpecialKey KeyUp)    Down _ _ -> state { selPlayer = Player1 }
            EventKey (SpecialKey KeyDown)  Down _ _ -> state { selPlayer = Player2 }
            EventKey (SpecialKey KeyEnter) Down _ _ -> unsafePerformIO $ do
                -- Apply chosen mode and join with chosen player, then enter game
                let modeMsg = SetMode (selMode state)
                void $ send sock (LBS.toStrict $ encode modeMsg)
                let joinMsg = JoinGame (selPlayer state)
                void $ send sock (LBS.toStrict $ encode joinMsg)
                pure state { myPlayerID = selPlayer state, uiPhase = InGame }
            _ -> state

        InGame -> case event of
            EventKey (Char 'q') Down _ _ -> unsafePerformIO exitSuccess
            -- Hold movement
            EventKey (Char 'w') Down _ _ -> sendAction sock pId MoveUp state
            EventKey (Char 's') Down _ _ -> sendAction sock pId MoveDown state
            EventKey (Char 'a') Down _ _ -> sendAction sock pId MoveLeft state
            EventKey (Char 'd') Down _ _ -> sendAction sock pId MoveRight state
            -- Release movement
            EventKey (Char 'w') Up _ _ -> sendAction sock pId Idle state
            EventKey (Char 's') Up _ _ -> sendAction sock pId Idle state
            EventKey (Char 'a') Up _ _ -> sendAction sock pId Idle state
            EventKey (Char 'd') Up _ _ -> sendAction sock pId Idle state
            -- Shoot
            EventKey (SpecialKey KeySpace) Down _ _ -> sendAction sock pId Shoot state
            EventKey (SpecialKey KeySpace) Up _ _   -> state
            -- Mode hotkeys in-game
            EventKey (Char '1') Down _ _ -> setMode sock Coop state
            EventKey (Char '2') Down _ _ -> setMode sock Solo state
            EventKey (Char 'n') Down _ _ -> setMode sock Coop state
            EventKey (Char 'b') Down _ _ -> setMode sock Solo state
            _ -> state

-- 3. HÀM UPDATE
updateHandler :: MVar GameState -> Float -> ClientState -> ClientState
updateHandler mvar _ currentState = unsafePerformIO $ do
    newGameState <- readMVar mvar
    return $ currentState { gameState = newGameState }

-- CÁC HÀM PHỤ TRỢ KHÁC VÀ LUỒNG MẠNG
sendAction :: Socket -> PlayerID -> Action -> ClientState -> ClientState
sendAction sock pId action state = unsafePerformIO $ do
    let msg = PlayerAction pId action
    void $ send sock (LBS.toStrict $ encode msg)
    return state

receiverLoop :: Socket -> MVar GameState -> IO ()
receiverLoop sock gameStateMVar = forever $ do
    byteString <- recv sock 1024 
    let serverMsg = decode (LBS.fromStrict byteString) :: ServerMessage
    case serverMsg of
        Welcome _ gs -> void $ swapMVar gameStateMVar gs
        UpdateGame gs -> void $ swapMVar gameStateMVar gs
        _ -> return ()

-- Đổi chế độ chơi (gửi lên server)
setMode :: Socket -> GameMode -> ClientState -> ClientState
setMode sock mode st = unsafePerformIO $ do
    let msg = SetMode mode
    void $ send sock (LBS.toStrict $ encode msg)
    return st