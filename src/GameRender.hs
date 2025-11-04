module GameRender where

import GameData
import Graphics.Gloss

-- TẠO MỘT KIỂU DỮ LIỆU ĐỂ GÓI CÁC SPRITE LẠI
data GameSprites = GameSprites
    { playerSprite :: Picture
    , enemySprite  :: Picture
    , bulletSprite :: Picture
    , itemSprite   :: Picture
    }

-- Chữ ký hàm Render chỉ cần nhận vào các sprite và GameState tổng thể
render :: GameSprites -> GameState -> Picture
render sprites gs = pictures
    [ drawPlayers   (playerSprite sprites) (gamePlayer gs)
    , drawEnemies   (enemySprite sprites)  (gameEnemies gs)
    , drawBullets   (bulletSprite sprites) (gameBullets gs)
    , drawItems     (itemSprite sprites)   (gameItems gs)
    , drawUI        gs
    ]

-- Vẽ Phi thuyền (Player)
drawPlayers :: Picture -> [Player] -> Picture
drawPlayers sprite ps = pictures (map (drawPlayer sprite) ps)

drawPlayer :: Picture -> Player -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawPlayer sprite p = let (Position x y) = playerPos p in translate x y $ scale 0.5 0.5 sprite

-- Vẽ Kẻ địch (Enemy)
drawEnemies :: Picture -> [Enemy] -> Picture
drawEnemies sprite es = pictures (map (drawEnemy sprite) es)

drawEnemy :: Picture -> Enemy -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawEnemy sprite e = let (Position x y) = enemyPos e in translate x y $ scale 1 1 sprite

-- Vẽ Đường đạn (Bullet)
drawBullets :: Picture -> [Bullet] -> Picture
drawBullets sprite bs = pictures (map (drawBullet sprite) bs)

drawBullet :: Picture -> Bullet -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawBullet sprite b = let (Position x y) = bulletPos b in translate x y $ scale 1 1 sprite

-- Vẽ Vật phẩm (Item)
drawItems :: Picture -> [Item] -> Picture
drawItems sprite is = pictures (map (drawItem sprite) is)

drawItem :: Picture -> Item -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawItem sprite i = let (Position x y) = itemPos i in translate x y $ scale 1 1 sprite

-- Vẽ Giao diện (UI)
drawUI :: GameState -> Picture
drawUI gs =
    case gamePlayer gs of
        (p:_) ->
            let -- Cập nhật UI cho cả 2 người chơi nếu có
                p1Score = maybe 0 playerScore (findPlayer Player1 gs)
                p1Lives = maybe 0 playerLives (findPlayer Player1 gs)
                p2Score = maybe 0 playerScore (findPlayer Player2 gs)
                p2Lives = maybe 0 playerLives (findPlayer Player2 gs)

                scoreTextP1 = "P1 Score: " ++ show p1Score ++ " | Lives: " ++ show p1Lives
                scoreTextP2 = "P2 Score: " ++ show p2Score ++ " | Lives: " ++ show p2Lives
                
                uiP1 = translate (-380) 280 $ scale 0.15 0.15 $ color white $ text scoreTextP1
                uiP2 = translate (-380) 260 $ scale 0.15 0.15 $ color white $ text scoreTextP2

            in pictures [uiP1, uiP2]
        [] -> blank

-- Hàm phụ trợ để tìm player
findPlayer :: PlayerID -> GameState -> Maybe Player
findPlayer pID gs = 
    let players = gamePlayer gs
        found = filter ((== pID) . playerID) players
    in case found of
        (p:_) -> Just p
        []    -> Nothing