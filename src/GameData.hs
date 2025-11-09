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

-- | Dữ liệu cho người chơi (ĐÃ SỬA)
-- | Trạng thái FSM cho Bot AI
data BotState
  = BotSpawn       -- vừa xuất hiện / respawn
  | BotCombat      -- đang chiến đấu bình thường (máu khỏe)
  | BotLowHealth   -- máu thấp, vẫn bắn nhưng thận trọng tìm item
  | BotSeekItem    -- ưu tiên di chuyển tới item để hồi phục
  | BotDead        -- đã chết, chờ hồi sinh
  deriving (Show, Eq, Generic)
instance Binary BotState

data Player = Player
  { playerID       :: PlayerID 
  , playerPos      :: Position     -- vị trí (x, y)
  , playerLives    :: Int          -- số mạng
  , playerScore    :: Int          -- điểm
  , playerDeaths   :: Int          -- số lần chết
  , playerAction   :: Action       -- hành động di chuyển hiện tại (Idle, MoveUp,...)
  , playerWantsToShoot :: Bool     -- cờ báo hiệu muốn bắn (do 'Shoot' là sự kiện 1 lần)
  , playerType     :: PlayerType   -- human or bot
  , playerShootCooldown :: Float   -- cooldown timer for shooting (giây)
  , playerRespawnTimer :: Float    -- respawn timer cho bot (giây), 0 = alive
  , playerDamageTaken :: Int       -- tổng HP đã mất (giảm khi ăn item)
  , playerBotState :: BotState     -- FSM state (Human: giữ BotCombat mặc định)
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
  , enemyFireCd   :: Float      -- cooldown bắn đạn (giây)
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
  { bulletPos     :: Position
  , bulletOwner   :: Owner
  , bulletShooter :: Maybe PlayerID
  , bulletDir     :: Float        -- ^ Direction multiplier: 1 = up, -1 = down
  } deriving (Show, Eq, Generic)
instance Binary Bullet

-- | Dữ liệu cho vật phẩm
data Item = Item
  { itemPos  :: Position
  , itemHeal :: Int
  } deriving (Show, Eq, Generic)
instance Binary Item

-- | Kiểu người chơi: human hay bot
data PlayerType = Human | Bot
  deriving (Show, Eq, Generic)
instance Binary PlayerType

-- | Chế độ game
-- Solo: chỉ 1 người chơi, không có bot (khó)
-- CoopBot: 1 người chơi + 1 AI bot cùng diệt quái (dễ)
-- Coop: 2 người chơi cùng diệt quái
-- PvP: 2 người chơi bắn nhau
data GameMode = Solo | CoopBot | Coop | PvP
  deriving (Show, Eq, Generic)
instance Binary GameMode

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
  , gameSpawnTimer     :: Float          -- bộ đếm spawn item
  , gameEnemySpawnTimer :: Float         -- bộ đếm spawn kẻ địch
  , gameBullets        :: [Bullet]      -- danh sách đạn
  , isShooting         :: Bool          -- có đang bắn không
  , gameMode           :: GameMode      -- chế độ chơi (Solo/CoopBot/Coop/PvP)
  , gameEnemiesEscaped :: Int           -- số enemy đã rơi xuống (thua khi >= 3)
  } deriving (Show, Eq, Generic)
instance Binary GameState