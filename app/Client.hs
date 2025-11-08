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
import Control.Monad (forever, void, when)
import Control.Concurrent.MVar

-- Game Modules
import GameMes
import GameData
import GameRender

-- Trạng thái tổng thể của Client, được 'gloss' quản lý
data UiPhase = InMenu | InGame | UiGameOver
    deriving (Eq, Show)

data ClientState = ClientState
    { gameState       :: GameState
    , networkSocket   :: Socket
    , myPlayerID      :: PlayerID
    , gameSprites     :: GameSprites
    , uiPhase         :: UiPhase
    , selMode         :: GameMode
    , selPlayer       :: PlayerID
    , gameOver        :: Bool
    , gameOverMVar    :: MVar Bool  -- Add reference to MVar
    , playerIDMVar    :: MVar PlayerID  -- Add MVar for actual PlayerID from server
    }

main :: IO ()
main = withSocketsDo $ do
    -- 0. PARSE COMMAND LINE ARGUMENTS FIRST
    args <- getArgs
    
    -- 1. TẢI HÌNH ẢNH
    putStrLn "Loading sprites..."
    playerPic <- loadJuicyPNG_ "assets/spaceship.png"
    enemyPic  <- loadJuicyPNG_ "assets/enemy.png"
    bulletPic <- loadJuicyPNG_ "assets/bullet.png"
    itemPic   <- loadJuicyPNG_ "assets/item.png"
    let sprites = GameSprites playerPic enemyPic bulletPic itemPic
    putStrLn "All sprites loaded."

    -- 2. THIẾT LẬP KẾT NỐI MẠNG
    -- Parse command line args for server IP (default: localhost)
    let serverIP = case args of
                     (ip:_) | not (null ip) && head ip /= 'p' -> ip  -- First arg is IP if doesn't start with 'p'
                     _      -> "127.0.0.1"  -- Default to localhost
    let serverPort = "8080"
    putStrLn $ "Connecting to server at " ++ serverIP ++ ":" ++ serverPort ++ "..."
    addrInfos <- getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just serverIP) (Just serverPort)
    sock <- case addrInfos of
        (ai:_) -> do
            s <- socket (addrFamily ai) Datagram defaultProtocol
            connect s (addrAddress ai)
            putStrLn "Network socket connected."
            return s
        [] -> error "No address info available"

    -- 3. KHỞI TẠO MVAR VÀ LUỒNG MẠNG
    --                Player Enemy Item Level Left Spwn Wins Loss ItemSpawn EnemySpawn Bullets Shoot Mode Escaped
    let initialGameState = GameState [] [] [] 0 0 0 0 0 5.0 2.0 [] False Coop 0
    
    gameStateMVar <- newMVar initialGameState
    gameOverMVar <- newMVar False
    playerIDMVar <- newMVar Player1  -- Will be updated when server sends Welcome
    _ <- forkIO $ receiverLoop sock gameStateMVar gameOverMVar playerIDMVar

    -- 4. XÁC ĐỊNH PLAYERID TỪ THAM SỐ DÒNG LỆNH
    -- Check if "player2" is in arguments
    let pID = if "player2" `elem` args then Player2 else Player1
    
    putStrLn $ "Ready. Opened Menu. Default selection: " ++ show pID
    
    let displayMode = InWindow "Haskell Shooter" (800, 600) (100, 100)
    
    let initialState = ClientState
            { gameState = initialGameState
            , networkSocket = sock
            , myPlayerID = pID
            , gameSprites = sprites
            , uiPhase = InMenu
            , selMode = Coop
            , selPlayer = pID
            , gameOver = False
            , gameOverMVar = gameOverMVar
            , playerIDMVar = playerIDMVar
            }

    let customBackground = makeColorI 25 25 112 255
    putStrLn "Starting game loop..."
    play displayMode customBackground 60 initialState drawHandler inputHandler (updateHandler gameStateMVar gameOverMVar playerIDMVar)

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
    InGame -> render (gameSprites state) (gameState state)
    UiGameOver -> renderGameOver (gameState state)

-- 2. HÀM INPUT
inputHandler :: Event -> ClientState -> ClientState
inputHandler event state =
    let sock = networkSocket state
        pId  = myPlayerID state
    in case uiPhase state of
        InMenu -> case event of
            EventKey (Char 'q') Down _ _ -> unsafePerformIO exitSuccess
            EventKey (SpecialKey KeyLeft)  Down _ _ -> state { selMode = case selMode state of
                                                                    Solo    -> PvP
                                                                    CoopBot -> Solo
                                                                    Coop    -> CoopBot
                                                                    PvP     -> Coop }
            EventKey (SpecialKey KeyRight) Down _ _ -> state { selMode = case selMode state of
                                                                    Solo    -> CoopBot
                                                                    CoopBot -> Coop
                                                                    Coop    -> PvP
                                                                    PvP     -> Solo }
            EventKey (SpecialKey KeyUp)    Down _ _ -> state { selPlayer = Player1 }
            EventKey (SpecialKey KeyDown)  Down _ _ -> state { selPlayer = Player2 }
            EventKey (SpecialKey KeyEnter) Down _ _ -> unsafePerformIO $ do
                -- Reset game over state before starting new game
                void $ swapMVar (gameOverMVar state) False
                -- Apply chosen mode and join with chosen player
                let modeMsg = SetMode (selMode state)
                void $ send sock (LBS.toStrict $ encode modeMsg)
                let joinMsg = JoinGame (selPlayer state)
                void $ send sock (LBS.toStrict $ encode joinMsg)
                pure state { myPlayerID = selPlayer state, uiPhase = InGame, gameOver = False }
            _ -> state

        InGame -> case event of
            EventKey (Char 'q') Down _ _ -> unsafePerformIO $ do
                let quitMsg = PlayerQuit pId
                void $ send sock (LBS.toStrict $ encode quitMsg)
                exitSuccess
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
            EventKey (Char '1') Down _ _ -> setMode sock Solo state
            EventKey (Char '2') Down _ _ -> setMode sock CoopBot state
            EventKey (Char '3') Down _ _ -> setMode sock Coop state
            EventKey (Char '4') Down _ _ -> setMode sock PvP state
            _ -> state

        UiGameOver -> case event of
            EventKey (SpecialKey KeyEnter) Down _ _ -> unsafePerformIO $ do
                -- Reset game over state in MVar
                void $ swapMVar (gameOverMVar state) False
                return $ state { uiPhase = InMenu, gameOver = False }
            EventKey (Char 'q') Down _ _ -> unsafePerformIO $ do
                let quitMsg = PlayerQuit pId
                void $ send sock (LBS.toStrict $ encode quitMsg)
                exitSuccess
            _ -> state

-- 3. HÀM UPDATE
updateHandler :: MVar GameState -> MVar Bool -> MVar PlayerID -> Float -> ClientState -> ClientState
updateHandler gsVar goVar pidVar _ currentState = unsafePerformIO $ do
    newGameState <- readMVar gsVar
    isOver <- readMVar goVar
    actualPlayerID <- readMVar pidVar  -- Read actual PlayerID from server
    let currentPhase = uiPhase currentState
        nextPhase 
          | currentPhase == InMenu = InMenu  -- Never auto-transition from menu
          | currentPhase == InGame && isOver = UiGameOver  -- Only transition to GameOver from InGame
          | otherwise = currentPhase
    return $ currentState { gameState = newGameState
                          , uiPhase = nextPhase
                          , gameOver = isOver
                          , myPlayerID = actualPlayerID  -- Update with server-assigned PlayerID
                          }

-- CÁC HÀM PHỤ TRỢ KHÁC VÀ LUỒNG MẠNG
sendAction :: Socket -> PlayerID -> Action -> ClientState -> ClientState
sendAction sock pId action state = unsafePerformIO $ do
    let msg = PlayerAction pId action
    void $ send sock (LBS.toStrict $ encode msg)
    return state

receiverLoop :: Socket -> MVar GameState -> MVar Bool -> MVar PlayerID -> IO ()
receiverLoop sock gameStateMVar gameOverMVar playerIDMVar = forever $ do
    byteString <- recv sock 1024 
    let sm = decode (LBS.fromStrict byteString) :: ServerMessage
    handleMsg sm
    where
        handleMsg :: ServerMessage -> IO ()
        handleMsg (Welcome pID gs) = do
            void $ swapMVar gameStateMVar gs
            void $ swapMVar playerIDMVar pID  -- Update actual PlayerID from server
            putStrLn $ "Server assigned us PlayerID: " ++ show pID
        handleMsg (UpdateGame gs)     = void $ swapMVar gameStateMVar gs
        handleMsg (UpdatePartial _ _) = return ()
        handleMsg (PlayerJoined _)    = return ()
        handleMsg (PlayerLeft _)      = return ()
        handleMsg (GameOver _)        = void $ swapMVar gameOverMVar True
        handleMsg (PlayerEliminated pid) = do
            me <- readMVar playerIDMVar
            when (pid == me) $ do
                void $ swapMVar gameOverMVar True

-- Đổi chế độ chơi (gửi lên server)
setMode :: Socket -> GameMode -> ClientState -> ClientState
setMode sock mode st = unsafePerformIO $ do
    let msg = SetMode mode
    void $ send sock (LBS.toStrict $ encode msg)
    return st