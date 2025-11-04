module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM_)
import qualified Data.Map as Map
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Data.Maybe (mapMaybe)

import GameData
import GameMes

-- Small tuple helpers
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

-- | Trạng thái của server, bao gồm trạng thái game và danh sách client
data ServerState = ServerState
    { gameState  :: TVar GameState
    , clients    :: TVar (Map.Map PlayerID SockAddr)
    }

-- | Hằng số cho game
playerSpeed, bulletSpeed, enemySpeed, entitySize :: Float
playerSpeed = 200.0 -- pixel mỗi giây
bulletSpeed = 400.0
enemySpeed  = 100.0
entitySize  = 20.0  -- Kích thước va chạm (giả định)

screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

-- | Hàm chính chạy logic game
-- | Trả về (GameState mới, Đạn vừa được bắn)
runGameLogic :: Float -> GameState -> (GameState, [Bullet])
runGameLogic deltaT gs =
    let
        -- 1. Cập nhật người chơi và tách riêng đạn mới
        (updatedPlayers, newPlayerBullets) = updatePlayers deltaT (gamePlayer gs)

        -- 2. Cập nhật kẻ địch và tách riêng đạn mới
        (updatedEnemies, newEnemyBullets) = updateEnemies deltaT (gameEnemies gs)

        -- 3. Cập nhật đạn cũ
        updatedOldBullets = updateBullets deltaT (gameBullets gs)

        -- 4. Tạo GameState tạm thời để kiểm tra va chạm
        tempGameState = gs
            { gamePlayer = updatedPlayers
            , gameEnemies = updatedEnemies
            , gameBullets = updatedOldBullets
            }

        -- 5. Xử lý va chạm (chỉ với đạn cũ)
        finalGameState = handleCollisions tempGameState

        -- 6. Trả về state đã xử lý và danh sách đạn mới
        allNewBullets = newPlayerBullets ++ newEnemyBullets

    in (finalGameState, allNewBullets)

-- | Cập nhật vị trí người chơi, xử lý bắn đạn
updatePlayers :: Float -> [Player] -> ([Player], [Bullet])
updatePlayers deltaT players =
    let (updatedPlayers, newBulletsLists) = unzip (map (updatePlayer deltaT) players)
        newBullets = concat newBulletsLists
    in (updatedPlayers, newBullets)

-- | Cập nhật cho từng người chơi
updatePlayer :: Float -> Player -> (Player, [Bullet])
updatePlayer deltaT p =
    let (dx, dy) = case playerAction p of
            MoveUp    -> (0, playerSpeed * deltaT)
            MoveDown  -> (0, -playerSpeed * deltaT)
            MoveLeft  -> (-playerSpeed * deltaT, 0)
            MoveRight -> (playerSpeed * deltaT, 0)
            _         -> (0, 0) -- Bao gồm cả Shoot và Idle

        (Position x y) = playerPos p
        newPos = Position (clamp (x + dx) (-screenWidth/2) (screenWidth/2))
                          (clamp (y + dy) (-screenHeight/2) (screenHeight/2))

        -- Tạo đạn nếu hành động là Shoot (gắn thông tin người bắn)
        newBullet = case playerAction p of
            Shoot -> [Bullet (Position x (y + entitySize)) PlayerOwned (Just $ playerID p)]
            _     -> []

        -- Nếu là bắn, reset về Idle (để không bắn liên tục)
        finalAction = if playerAction p == Shoot then Idle else playerAction p

    in (p { playerPos = newPos, playerAction = finalAction }, newBullet)

-- | Cập nhật kẻ địch
updateEnemies :: Float -> [Enemy] -> ([Enemy], [Bullet])
updateEnemies deltaT enemies =
    let updatedEnemies = map (updateEnemy deltaT) enemies
        newBullets = []
    in (updatedEnemies, newBullets)

updateEnemy :: Float -> Enemy -> Enemy
updateEnemy deltaT e =
    let (Position x y) = enemyPos e
        newPos = Position x (y - enemySpeed * deltaT)
    in e { enemyPos = newPos }

-- | Cập nhật đạn (di chuyển, xóa đạn ngoài màn hình)
updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets deltaT bullets =
    mapMaybe (updateBullet deltaT) bullets

updateBullet :: Float -> Bullet -> Maybe Bullet
updateBullet deltaT b =
    let (Position x y) = bulletPos b
        dy = case bulletOwner b of
                PlayerOwned -> bulletSpeed * deltaT
                EnemyOwned  -> -bulletSpeed * deltaT
        newPos = Position x (y + dy)
    in
    if abs (posY newPos) > screenHeight / 2
    then Nothing
    else Just (b { bulletPos = newPos })

-- | Xử lý va chạm
handleCollisions :: GameState -> GameState
handleCollisions gs =
    let players = gamePlayer gs
        enemies = gameEnemies gs
        bullets = gameBullets gs

        playerBullets = filter ((== PlayerOwned) . bulletOwner) bullets
        enemyBullets  = filter ((== EnemyOwned) . bulletOwner) bullets

    -- 1. Đạn người chơi vs Kẻ địch
    tripleHit = checkBulletEnemyCollisions playerBullets enemies
    hitEnemies = fst3 tripleHit
    hitPlayerBullets = snd3 tripleHit
    remainingPlayerBullets = thd3 tripleHit

        -- 2. Đạn kẻ địch vs Người chơi
        (hitPlayers, remainingEnemyBullets) = checkBulletPlayerCollisions enemyBullets players

        -- Lọc kẻ địch
        updatedEnemies = filter (`notElem` hitEnemies) enemies

        -- Lấy ID của người chơi bị trúng đạn
        hitPlayerIDs = map playerID hitPlayers

        -- Cập nhật mạng người chơi
        updatedPlayers = map (\p -> if playerID p `elem` hitPlayerIDs
                                      then p { playerLives = playerLives p - 1 }
                                      else p) players

        -- Lọc ra người chơi còn sống
        alivePlayers = filter ((> 0) . playerLives) updatedPlayers

        -- Cập nhật điểm: gán điểm cho người bắn tương ứng với mỗi kẻ địch trúng
    -- Chỉ tính từ các viên đạn thực sự trúng kẻ địch
    shooterHits = foldr (\b acc -> case bulletShooter b of
                      Just pid -> Map.insertWith (+) pid 1 acc
                      Nothing  -> acc)
                Map.empty hitPlayerBullets

        -- Áp dụng điểm cho người chơi sống
        scoredPlayers = map (\p -> let add = Map.findWithDefault 0 (playerID p) shooterHits
                                    in p { playerScore = playerScore p + add }) alivePlayers

    in gs { gamePlayer = scoredPlayers
          , gameEnemies = updatedEnemies
          , gameBullets = remainingPlayerBullets ++ remainingEnemyBullets
          }

-- | Kiểm tra va chạm đạn người chơi và kẻ địch
checkBulletEnemyCollisions :: [Bullet] -> [Enemy] -> ([Enemy], [Bullet], [Bullet])
checkBulletEnemyCollisions bullets enemies =
    let collisions = [(b, e) | b <- bullets, e <- enemies, isColliding (bulletPos b) (enemyPos e) entitySize]
        hitEnemies = map snd collisions
        hitBullets = map fst collisions
        remainingBullets = filter (`notElem` hitBullets) bullets
    in (hitEnemies, hitBullets, remainingBullets)

-- | Kiểm tra va chạm đạn kẻ địch và người chơi
checkBulletPlayerCollisions :: [Bullet] -> [Player] -> ([Player], [Bullet])
checkBulletPlayerCollisions bullets players =
    let collisions = [(b, p) | b <- bullets, p <- players, isColliding (bulletPos b) (playerPos p) entitySize]
        hitPlayers = map snd collisions
        hitBullets = map fst collisions
        remainingBullets = filter (`notElem` hitBullets) bullets
    in (hitPlayers, remainingBullets)

-- | Hàm kiểm tra va chạm AABB đơn giản
isColliding :: Position -> Position -> Float -> Bool
isColliding (Position x1 y1) (Position x2 y2) size =
    abs (x1 - x2) < size && abs (y1 - y2) < size

-- | Hàm tiện ích để giới hạn giá trị
clamp :: (Ord a) => a -> a -> a -> a
clamp val minVal maxVal = max minVal (min maxVal val)