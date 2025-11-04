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
drawPlayer sprite p = let (Position x y) = playerPos p in translate x y $ scale 2 2 sprite

-- Vẽ Kẻ địch (Enemy)
drawEnemies :: Picture -> [Enemy] -> Picture
drawEnemies sprite es = pictures (map (drawEnemy sprite) es)

drawEnemy :: Picture -> Enemy -> Picture
drawEnemy sprite e = let (Position x y) = enemyPos e in translate x y $ scale 2 2 sprite

-- Vẽ Đường đạn (Bullet)
drawBullets :: Picture -> [Bullet] -> Picture
drawBullets sprite bs = pictures (map (drawBullet sprite) bs)

drawBullet :: Picture -> Bullet -> Picture
drawBullet sprite b = let (Position x y) = bulletPos b in translate x y $ scale 2 2 sprite

-- Vẽ Vật phẩm (Item)
drawItems :: Picture -> [Item] -> Picture
drawItems sprite is = pictures (map (drawItem sprite) is)

drawItem :: Picture -> Item -> Picture
drawItem sprite i = let (Position x y) = itemPos i in translate x y $ scale 2 2 sprite

-- Vẽ Giao diện (UI)
drawUI :: GameState -> Picture
drawUI gs =
    case gamePlayer gs of
        (p:_) ->
            let scoreText = "Score: " ++ show (playerScore p) ++ 
                            " | Lives: " ++ show (playerLives p)
            in translate (-380) 280 $ scale 0.15 0.15 $ color white $ text scoreText
        [] -> blank
