-- app/Server.hs

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM_)
import qualified Data.Map as Map
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)

import GameData
import GameMes

-- | Trạng thái của server, bao gồm trạng thái game và danh sách client
data ServerState = ServerState
  { gameState  :: TVar GameState
  , clients    :: TVar (Map.Map PlayerID SockAddr)
  }

-- | Hàm main để khởi chạy server
main :: IO ()
main = withSocketsDo $ do
    -- Cài đặt địa chỉ và cổng
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just "8080")

    -- Tạo socket
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    putStrLn "Server is listening on port 8080"

    -- Khởi tạo trạng thái ban đầu của game và server
    let initialGameState = GameState [] [] [] 1 0 0 0 0 False
    st <- atomically $ newTVar initialGameState
    cl <- atomically $ newTVar Map.empty
    let serverState = ServerState st cl

    -- Chạy vòng lặp game trong một luồng riêng
    _ <- forkIO $ gameLoop serverState sock

    -- Lắng nghe tin nhắn từ client
    listenLoop sock serverState

-- | Vòng lặp lắng nghe và xử lý tin nhắn từ các client
listenLoop :: Socket -> ServerState -> IO ()
listenLoop sock serverState = forever $ do
    (byteString, clientAddr) <- recvFrom sock 1024
    let clientMsg = decode (LBS.fromStrict byteString) :: ClientMessage
    
    -- Xử lý tin nhắn trong một luồng riêng để không block
    forkIO $ handleMessage clientMsg clientAddr sock serverState

-- | Xử lý các loại tin nhắn khác nhau từ client
handleMessage :: ClientMessage -> SockAddr -> Socket -> ServerState -> IO ()
handleMessage msg addr sock state = do
    putStrLn $ "Received message: " ++ show msg
    case msg of
        JoinGame pId -> do
            let newPlayer = Player pId (Position 0 0) 3 0 0 Idle
            currentGameState <- atomically $ readTVar (gameState state)
            
            -- Thêm người chơi mới và cập nhật trạng thái
            let updatedPlayers = newPlayer : gamePlayer currentGameState
            let newGameState = currentGameState { gamePlayer = updatedPlayers }
            
            atomically $ do
                writeTVar (gameState state) newGameState
                modifyTVar' (clients state) (Map.insert pId addr)
            
            -- Gửi tin nhắn chào mừng đến client vừa kết nối
            let welcomeMsg = Welcome pId newGameState
            sendTo sock (LBS.toStrict $ encode welcomeMsg) addr
            
            -- Thông báo cho các client khác
            broadcast (PlayerJoined pId) state sock (Just pId)

        PlayerMove pId pos -> do
            -- (Nâng cao) Cập nhật vị trí người chơi và broadcast
            putStrLn $ "Player " ++ show pId ++ " moved to " ++ show pos
            -- Logic cập nhật trạng thái game ở đây...
            
        PlayerQuit pId -> do
            atomically $ modifyTVar' (clients state) (Map.delete pId)
            -- Logic xóa người chơi khỏi GameState ở đây...
            broadcast (PlayerLeft pId) state sock Nothing

        _ -> return () -- Xử lý các message khác nếu cần

-- | Vòng lặp chính của game, chạy mỗi 16ms (~60 FPS)
gameLoop :: ServerState -> Socket -> IO ()
gameLoop state sock = forever $ do
    -- Đọc trạng thái game hiện tại
    gs <- atomically $ readTVar (gameState state)

    -- TODO: Cập nhật logic game ở đây
    -- Ví dụ: di chuyển kẻ địch, xử lý va chạm, sinh vật phẩm, ...
    let updatedGameState = gs -- Hiện tại chưa có logic, chỉ là ví dụ

    -- Ghi lại trạng thái mới
    atomically $ writeTVar (gameState state) updatedGameState
    
    -- Gửi trạng thái mới đến tất cả client
    broadcast (UpdateGame updatedGameState) state sock Nothing

    -- Delay một khoảng thời gian
    threadDelay 16000 -- 16ms

-- | Gửi một tin nhắn đến tất cả các client (hoặc tất cả trừ một người)
broadcast :: ServerMessage -> ServerState -> Socket -> Maybe PlayerID -> IO ()
broadcast msg state sock excludePlayerId = do
    clientMap <- atomically $ readTVar (clients state)
    let targetClients = case excludePlayerId of
            Just pId -> Map.delete pId clientMap
            Nothing  -> clientMap
            
    forM_ (Map.elems targetClients) $ \addr ->
        sendTo sock (LBS.toStrict $ encode msg) addr