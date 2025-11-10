module Main where

-- Gloss & System
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

-- Network & Concurrency
import Network.Socket
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Concurrent.MVar

-- Game Modules
import Game.Data
import Game.Messaging
import Game.Constants
import Client.Render
import Client.Input
import Client.Network
import Client.State

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
    let serverIP = case args of
                     (ip:_) | not (null ip) && head ip /= 'p' -> ip
                     _      -> "127.0.0.1"
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
    gameStateMVar <- newMVar initialGameState
    gameOverMVar <- newMVar False
    playerIDMVar <- newMVar Player1
    _ <- forkIO $ receiverLoop sock gameStateMVar gameOverMVar playerIDMVar

    -- 4. XÁC ĐỊNH PLAYERID TỪ THAM SỐ DÒNG LỆNH
    let pID = if "player2" `elem` args then Player2 else Player1
    
    putStrLn $ "Ready. Opened Menu. Default selection: " ++ show pID
    
    let displayMode = InWindow "Haskell Shooter" (round screenWidth, round screenHeight) (100, 100)
    
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
    play displayMode customBackground 60 initialState
        drawHandler                   -- Hàm vẽ cục bộ chọn theo UiPhase
        Client.Input.inputHandler     -- Hàm input từ Client.Input
        (updateHandler gameStateMVar gameOverMVar playerIDMVar) -- Hàm update (cục bộ)

----------------------------------------------------
-- HÀM UPDATE CỦA GLOSS
----------------------------------------------------

-- Hàm này vẫn ở đây vì nó làm cầu nối giữa I/O (MVar) và state (Gloss)
updateHandler :: MVar GameState -> MVar Bool -> MVar PlayerID -> Float -> ClientState -> ClientState
updateHandler gsVar goVar pidVar _ currentState = unsafePerformIO $ do
    newGameState <- readMVar gsVar
    isOver <- readMVar goVar
    actualPlayerID <- readMVar pidVar  -- Read actual PlayerID from server
    let currentPhase = uiPhase currentState
        nextPhase 
          | currentPhase == InMenu = InMenu
          | currentPhase == InGame && isOver = UiGameOver
          | otherwise = currentPhase
    return $ currentState { gameState = newGameState
                          , uiPhase = nextPhase
                          , gameOver = isOver
                          , myPlayerID = actualPlayerID
                          }

----------------------------------------------------
-- HÀM VẼ CỦA GLOSS (chọn theo UiPhase)
----------------------------------------------------

drawHandler :: ClientState -> Picture
drawHandler cs = case uiPhase cs of
    InMenu     -> Client.Render.renderMenu (selMode cs) (selPlayer cs)
    UiGameOver -> Client.Render.renderGameOver (gameState cs)
    InGame     -> Client.Render.render (gameSprites cs) (gameState cs)