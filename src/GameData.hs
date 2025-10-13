{-# LANGUAGE DeriveGeneric #-}

module GameData where

import GHC.Generics (Generic)
import Data.Binary (Binary)

-- | Vị trí 2D trong không gian game
data Position = Position
  { posX :: Float
  , posY :: Float
  } deriving (Show, Eq, Generic)
instance Binary Position

-- | Các hành động có thể thực hiện (player/enemy)
data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Shoot
  | Idle
  deriving (Show, Eq, Generic)
instance Binary Action

-- | Trạng thái sinh tồn (player/enemy)
data LifeState = Alive | Dead
  deriving (Show, Eq, Generic)
instance Binary LifeState

-- | Net: Định danh người chơi
data PlayerID 
  = Player1 
  | Player2
  deriving (Show, Eq, Ord, Generic)
instance Binary PlayerID

-- | Dữ liệu cho người chơi
data Player = Player
  { playerID       :: PlayerID 
  , playerPos      :: Position     -- vị trí (x, y)
  , playerLives    :: Int          -- số mạng
  , playerScore    :: Int          -- điểm
  , playerDeaths   :: Int          -- số lần chết
  , playerAction   :: Action       -- hành động hiện tại
  } deriving (Show, Eq, Generic)
instance Binary Player

-- | Loại kẻ địch
data EnemyType 
  = SmallEnemy 
  | BigEnemy
  deriving (Show, Eq, Generic)
instance Binary EnemyType

-- | Dữ liệu cho kẻ địch
data Enemy = Enemy
  { enemyPos      :: Position
  , enemyType     :: EnemyType
  , enemyAction   :: Action
  , enemyState    :: LifeState
  , enemyHP       :: Int
  } deriving (Show, Eq, Generic)
instance Binary Enemy

-- | Chủ sở hữu viên đạn
data Owner 
  = PlayerOwned 
  | EnemyOwned
  deriving (Show, Eq, Generic)
instance Binary Owner

-- | Dữ liệu cho viên đạn
data Bullet = Bullet
  { bulletPos   :: Position
  , bulletOwner :: Owner
  } deriving (Show, Eq, Generic)
instance Binary Bullet

-- | Dữ liệu cho vật phẩm
data Item = Item
  { itemPos  :: Position
  , itemHeal :: Int
  } deriving (Show, Eq, Generic)
instance Binary Item

-- | Dữ liệu tổng hợp trạng thái toàn bộ game
data GameState = GameState
  { gamePlayer       :: [Player]        -- người chơi chính
  , gameEnemies      :: [Enemy]         -- danh sách kẻ địch
  , gameItems        :: [Item]          -- danh sách vật phẩm
  , gameLevel        :: Int             -- số thứ tự màn
  , gameEnemiesLeft  :: Int             -- số lượng kẻ địch còn lại
  , gameEnemiesSpawned :: Int           -- số lượng đã sinh ra
  , gameWins          :: Int            -- số lần thắng
  , gameLosses        :: Int            -- số lần thua
  , gameBullets        :: [Bullet]           -- số lượng đạn
  , isShooting         :: Bool          -- có đang bắn không
  } deriving (Show, Eq, Generic)
instance Binary GameState