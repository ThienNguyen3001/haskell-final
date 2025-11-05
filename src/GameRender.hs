module GameRender where

import GameData
import Graphics.Gloss

-- Screen and layout constants
screenW, screenH :: Float
screenW = 800
screenH = 600

margin :: Float
margin = 14

scalePlayer, scaleEnemy, scaleBullet, scaleItem :: Float
scalePlayer = 0.05
scaleEnemy  = 0.05
scaleBullet = 0.05
scaleItem   = 0.05

-- TẠO MỘT KIỂU DỮ LIỆU ĐỂ GÓI CÁC SPRITE LẠI
data GameSprites = GameSprites
    { playerSprite :: Picture
    , enemySprite  :: Picture
    , bulletSprite :: Picture
    , itemSprite   :: Picture
    }

-- Chữ ký hàm Render chỉ cần nhận vào các sprite và GameState tổng thể
render :: GameSprites -> GameState -> (Int, Int) -> Picture
render sprites gs (winW, winH) = pictures
    [ drawPlayfieldBg
    , drawItems     (itemSprite sprites)   (gameItems gs)
    , drawEnemies   (enemySprite sprites)  (gameEnemies gs)
    , drawPlayers   (playerSprite sprites) (gamePlayer gs)
    , drawBullets   (bulletSprite sprites) (gameBullets gs)
    , drawHUD        gs                    (winW, winH)
    ]

-- Subtle playfield background
drawPlayfieldBg :: Picture
drawPlayfieldBg =
    let bgW = screenW * 4  -- large enough to cover typical fullscreen resolutions
        bgH = screenH * 4
    in color (makeColorI 20 50 20 250)
         $ rectangleSolid (bgW - 2*margin) (bgH - 2*margin)

-- Vẽ Phi thuyền (Player)
drawPlayers :: Picture -> [Player] -> Picture
drawPlayers sprite ps = pictures (map (drawPlayer sprite) ps)

drawPlayer :: Picture -> Player -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawPlayer sprite p = let (Position x y) = playerPos p in translate x y $ scale scalePlayer scalePlayer sprite

-- Vẽ Kẻ địch (Enemy)
drawEnemies :: Picture -> [Enemy] -> Picture
drawEnemies sprite es = pictures (map (drawEnemy sprite) es)

drawEnemy :: Picture -> Enemy -> Picture
drawEnemy sprite e =
    let (Position x y) = enemyPos e
    in translate x y $ scale scaleEnemy scaleEnemy sprite

-- Vẽ Đường đạn (Bullet)
drawBullets :: Picture -> [Bullet] -> Picture
drawBullets sprite bs = pictures (map (drawBullet sprite) bs)

drawBullet :: Picture -> Bullet -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawBullet sprite b = let (Position x y) = bulletPos b in translate x y $ scale scaleBullet scaleBullet sprite

-- Vẽ Vật phẩm (Item)
drawItems :: Picture -> [Item] -> Picture
drawItems sprite is = pictures (map (drawItem sprite) is)

drawItem :: Picture -> Item -> Picture
-- SỬA Ở ĐÂY: scale 2 2 -> scale 1 1
drawItem sprite i = let (Position x y) = itemPos i in translate x y $ scale scaleItem scaleItem sprite

-- HUD / UI Overlay
drawHUD :: GameState -> (Int, Int) -> Picture
drawHUD gs (winW, winH) = pictures [topBar, playersPanel, footer]
    where
        w = fromIntegral winW
        h = fromIntegral winH
        modeText = case gameMode gs of
            Coop -> "Mode: Coop"
            Solo -> "Mode: Solo"
        p1Lives = maybe 0 playerLives (findPlayer Player1 gs)
        p2Lives = maybe 0 playerLives (findPlayer Player2 gs)
        topText = modeText ++ "  |  Lives: P1 " ++ show p1Lives ++ "  P2 " ++ show p2Lives
        topBar = translate (-w/2 + margin) (h/2 - margin - 18)
            $ scale 0.12 0.12
            $ color (makeColorI 220 220 255 255)
            $ text topText

        -- Players panel (scores + lives bars)
        playersPanel = pictures
            [ drawPlayerHud Player1 (-screenW/2 + margin) (screenH/2 - 60) gs
            , drawPlayerHud Player2 (-screenW/2 + margin) (screenH/2 - 90) gs
            ]

        -- Footer with controls
        footer = translate (-w/2 + margin) (-h/2 + margin)
                     $ scale 0.1 0.1
                     $ color (makeColorI 200 200 200 255)
                     $ text "WASD move, Space shoot | 1/n: Coop, 2/b: Solo | q: quit"

drawPlayerHud :: PlayerID -> Float -> Float -> GameState -> Picture
drawPlayerHud pid x y gs =
    case findPlayer pid gs of
        Nothing -> blank
        Just p  ->
            let lbl   = (if pid == Player1 then "P1" else "P2") ++ ("  Score: " ++ show (playerScore p))
                lives = playerLives p
            in pictures [ translate x y $ scale 0.12 0.12 $ color white $ text lbl
                        , translate (x+220) (y-4) $ drawLivesBar lives
                        ]

drawLivesBar :: Int -> Picture
drawLivesBar n =
    let maxLives = max 1 n
        w = 120
        h = 10
        filled = fromIntegral (min n maxLives) / fromIntegral maxLives
        back = color (greyN 0.2) $ rectangleSolid w h
        fore = color (makeColorI 0 200 90 255)
             $ translate (-(w/2) + (w*filled)/2) 0
             $ rectangleSolid (w*filled) h
    in pictures [back, fore]

-- Menu rendering (mode & player selection)
renderMenu :: GameMode -> PlayerID -> Picture
renderMenu selMode selPlayer = pictures
    [ translate (-220) 140 $ scale 0.25 0.25 $ color (makeColorI 230 230 255 255) $ text "Haskell Shooter"
    , translate (-150) 60  $ scale 0.15 0.15 $ color (greyN 0.8) $ text "Select Mode"
    , translate (-150) 30  $ selectable (selMode == Coop) "Coop"
    , translate (-150) 0   $ selectable (selMode == Solo) "Solo"
    , translate (-150) (-40) $ scale 0.15 0.15 $ color (greyN 0.8) $ text "Select Player"
    , translate (-150) (-70) $ selectable (selPlayer == Player1) "Player 1"
    , translate (-150) (-100) $ selectable (selPlayer == Player2) "Player 2"
    , translate (-300) (-200) $ scale 0.12 0.12 $ color (makeColorI 150 200 200 255)
                                                    $ text "Left/Right: Mode  |  Up/Down: Player  |  Enter: Start  |  Q: Quit"
    ]
    where
        selectable on label =
            let c = if on then makeColorI 255 210 80 255 else white
            in scale 0.18 0.18 $ color c $ text (prefix on ++ label)
        prefix True  = "> "
        prefix False = "  "

-- Hàm phụ trợ để tìm player
findPlayer :: PlayerID -> GameState -> Maybe Player
findPlayer pID gs = 
    let players = gamePlayer gs
        found = filter ((== pID) . playerID) players
    in case found of
        (p:_) -> Just p
        []    -> Nothing