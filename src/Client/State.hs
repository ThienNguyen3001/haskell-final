module Client.State where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Network.Socket
import Control.Concurrent.MVar

import Game.Data
import Client.Render (GameSprites(..)) -- Import kiểu dữ liệu GameSprites

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
    , gameOverMVar    :: MVar Bool
    , playerIDMVar    :: MVar PlayerID
    }

-- Hàm load ảnh phụ trợ để xử lý lỗi
loadJuicyPNG_ :: FilePath -> IO Picture
loadJuicyPNG_ path = do
    maybePic <- loadJuicyPNG path
    case maybePic of
        Just pic -> return pic
        Nothing  -> do
            putStrLn $ "Cannot load image: " ++ path
            return blank