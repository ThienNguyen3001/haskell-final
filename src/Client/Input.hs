module Client.Input where

import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket
import qualified Network.Socket.ByteString as NBS (send)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode)
import Control.Concurrent.MVar
import Control.Monad (void)

import Game.Data
import Game.Messaging
import Client.State -- Import ClientState

-- HÀM INPUT
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
                void $ NBS.send sock (LBS.toStrict $ encode modeMsg)
                let joinMsg = JoinGame (selPlayer state)
                void $ NBS.send sock (LBS.toStrict $ encode joinMsg)
                pure state { myPlayerID = selPlayer state, uiPhase = InGame, gameOver = False }
            _ -> state

        InGame -> case event of
            EventKey (Char 'q') Down _ _ -> unsafePerformIO $ do
                let quitMsg = PlayerQuit pId
                void $ NBS.send sock (LBS.toStrict $ encode quitMsg)
                exitSuccess
            -- Return to menu (offline modes only)
            EventKey (Char 'r') Down _ _ ->
                let mode = gameMode (gameState state)
                in if mode == Solo || mode == CoopBot
                      then unsafePerformIO $ do
                          -- reset any game over state and go back to menu locally
                          void $ swapMVar (gameOverMVar state) False
                          pure $ state { uiPhase = InMenu, gameOver = False }
                      else state
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
                void $ NBS.send sock (LBS.toStrict $ encode quitMsg)
                exitSuccess
            _ -> state

-- CÁC HÀM PHỤ TRỢ
sendAction :: Socket -> PlayerID -> Action -> ClientState -> ClientState
sendAction sock pId action state = unsafePerformIO $ do
    let msg = PlayerAction pId action
    void $ NBS.send sock (LBS.toStrict $ encode msg)
    return state

-- Đổi chế độ chơi (gửi lên server)
setMode :: Socket -> GameMode -> ClientState -> ClientState
setMode sock mode st = unsafePerformIO $ do
    let msg = SetMode mode
    void $ NBS.send sock (LBS.toStrict $ encode msg)
    return st