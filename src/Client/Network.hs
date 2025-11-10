module Client.Network where

import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NBS (recv)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Binary (decode)
import Control.Monad (forever, void, when)
import Control.Concurrent.MVar
import Control.Exception (try, SomeException, evaluate)

import Game.Data
import Game.Messaging

receiverLoop :: Socket -> MVar GameState -> MVar Bool -> MVar PlayerID -> IO ()
receiverLoop sock gameStateMVar gameOverMVar playerIDMVar = forever loop
  where
    loop = do
      -- Receive a full UDP datagram with a large enough buffer to avoid truncation
      eBs <- try (NBS.recv sock 65535) :: IO (Either SomeException BS.ByteString)
      case eBs of
          Left err -> do
              putStrLn $ "recv error: " ++ show err
              return ()
          Right bs -> do
              eMsg <- try (evaluate (decode (LBS.fromStrict bs) :: ServerMessage))
              case eMsg of
                  Left err -> putStrLn $ "decode error: " ++ show (err :: SomeException)
                  Right sm -> handleMsg sm

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