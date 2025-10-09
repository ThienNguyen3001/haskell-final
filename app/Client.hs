module Client where

import Network.Socket
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Concurrent.MVar

import GameMes
import GameData

runClient :: HostName -> ServiceName -> IO ()
runClient host port = withSocketsDo $ do
    -- PHASE 1: CONNECTION
    addrInfos <- getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect sock (addrAddress serverAddr)
    putStrLn $ "Connected to " ++ host ++ ":" ++ port

    let initialGameState = GameState [] [] [] 1 0 0 0 0 False
    gameStateMVar <- newMVar initialGameState
    _ <- forkIO $ receiverLoop sock gameStateMVar

    -- PHASE 2: SEND MESSAGE
    putStrLn "Sending JoinGame message..."
    let joinMsg = JoinGame Player1 -- Giả sử client này là Player1
    send sock (LBS.toStrict $ encode joinMsg) -- Serialize và gửi đi
    
    putStrLn "Client is running. Press Enter to quit."
    _ <- getLine

    close sock
    putStrLn "Connection closed."

-- | Vòng lặp liên tục nhận và xử lý tin nhắn từ server
receiverLoop :: Socket -> MVar GameState -> IO ()
receiverLoop sock gameStateMVar = forever $ do
    byteString <- recv sock 1024 -- max 1024 bytes
    
    -- Deserialize
    let serverMsg = decode (LBS.fromStrict byteString) :: ServerMessage

    case serverMsg of
        -- Khi server chào mừng hoặc cập nhật, ghi đè GameState trong MVar
        Welcome _ newGameState -> do
            putStrLn "Received Welcome! Updating state."
            _ <- swapMVar gameStateMVar newGameState -- swapMVar an toàn hơn
        UpdateGame newGameState -> do
            putStrLn "Received game update." -- Để debug, có thể bỏ để đỡ spam console
            _ <- swapMVar gameStateMVar newGameState
        -- Xử lý các thông điệp khác nếu cần
        PlayerJoined pId -> putStrLn $ show pId ++ " has joined."
        PlayerLeft pId -> putStrLn $ show pId ++ " has left."
        GameOver isWin -> putStrLn $ "Game Over! Win: " ++ show isWin
        _ -> return ()

-- | Hàm main để chạy client
main :: IO ()
main = do
    let serverIP = "127.0.0.1" -- Địa chỉ IP của server (localhost)
    let serverPort = "8080"      -- Cổng của server
    runClient serverIP serverPort