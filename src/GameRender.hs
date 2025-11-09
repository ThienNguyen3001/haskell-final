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
render :: GameSprites -> GameState -> Picture
render sprites gs = pictures
    [ drawPlayfieldBg
    , drawItems     (itemSprite sprites)   (gameItems gs)
    , drawEnemies   (enemySprite sprites)  (gameEnemies gs)
    , drawPlayers   (gameMode gs) (playerSprite sprites) (gamePlayer gs)
    , drawBullets   (bulletSprite sprites) (gameBullets gs)
    , drawHUD       gs
    ]

-- Game Over screen
renderGameOver :: GameState -> Picture
renderGameOver gs = pictures
    [ color (makeColorI 0 0 0 180) $ rectangleSolid (screenW*1.2) (screenH*1.2)
    , translate (-220) 80 $ scale 0.3 0.3 $ color (makeColorI 255 80 80 255) $ text "GAME OVER"
    , translate (-180) 20 $ scale 0.15 0.15 $ color (makeColorI 255 150 150 255) $ text reason
    , translate (-250) (-40) $ scale 0.15 0.15 $ color white $ text (scoresText gs)
    , translate (-250) (-100) $ scale 0.12 0.12 $ color (makeColorI 200 200 200 255) $ text "Enter: Menu  |  Q: Quit"
    ]
    where
        reason = if gameEnemiesEscaped gs >= 3
                 then "Too many enemies escaped!"
                 else "All players defeated!"

scoresText :: GameState -> String
scoresText st = "Score P1: "
             ++ show (maybe 0 playerScore (findPlayer Player1 st))
             ++ "  P2: "
             ++ show (maybe 0 playerScore (findPlayer Player2 st))

-- Subtle playfield background
drawPlayfieldBg :: Picture
drawPlayfieldBg =
    let bgW = screenW * 4  -- large enough to cover typical fullscreen resolutions
        bgH = screenH * 4
    in color (makeColorI 20 50 20 250)
         $ rectangleSolid (bgW - 2*margin) (bgH - 2*margin)

-- Vẽ Phi thuyền (Player)
drawPlayers :: GameMode -> Picture -> [Player] -> Picture
drawPlayers mode sprite ps = pictures (map (drawPlayer mode sprite) ps)

drawPlayer :: GameMode -> Picture -> Player -> Picture
drawPlayer mode sprite p =
    let Position x y = playerPos p
        oriented = case mode of
                      PvP | playerID p == Player2 -> rotate 180 sprite
                      _                            -> sprite
    in translate x y $ scale scalePlayer scalePlayer oriented

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
drawHUD :: GameState -> Picture
drawHUD gs = pictures [topBar, playersPanel, footer]
    where
        -- Top bar with mode and escaped counter
        modeText = case gameMode gs of
            Solo     -> "SOLO"
            CoopBot  -> "COOP+BOT"
            Coop     -> "COOP"
            PvP      -> "PVP"
        topText = "Mode: " ++ modeText ++ "  |  Enemies Escaped: " ++ show (gameEnemiesEscaped gs) ++ "/3"
        topBar = translate (-screenW/2 + margin) (screenH/2 - margin - 18)
            $ scale 0.12 0.12
            $ color (makeColorI 220 220 255 255)
            $ text topText

    -- Players panel (scores only)
        playersPanel = pictures
            [ drawPlayerHud Player1 (-screenW/2 + margin) (screenH/2 - 60) gs
            , drawPlayerHud Player2 (-screenW/2 + margin) (screenH/2 - 90) gs
            ]

        -- Footer with controls
        footer = translate (-screenW/2 + margin) (-screenH/2 + margin)
                     $ scale 0.1 0.1
                     $ color (makeColorI 200 200 200 255)
                     $ text "WASD move, Space shoot | 1: Solo, 2: CoopBot, 3: Coop, 4: PvP | q: quit"

drawPlayerHud :: PlayerID -> Float -> Float -> GameState -> Picture
drawPlayerHud pid x y gs =
    case findPlayer pid gs of
        Nothing -> blank
        Just p  ->
            let playerLabel = if pid == Player1 then "P1" else "P2"
                livesText   = "  Lives: " ++ show (playerLives p)
                scoreText   = "  Score: " ++ show (playerScore p)
                lbl = playerLabel ++ livesText ++ scoreText
            in translate x y $ scale 0.12 0.12 $ color white $ text lbl

-- Lives bar removed per request

-- Menu rendering (mode & player selection) - Enhanced
renderMenu :: GameMode -> PlayerID -> Picture
renderMenu selMode selPlayer = pictures
    [ -- Title with shadow effect
      translate (-218) 138 $ scale 0.25 0.25 $ color (greyN 0.3) $ text "Haskell Shooter"
    , translate (-220) 140 $ scale 0.25 0.25 $ color (makeColorI 80 200 255 255) $ text "Haskell Shooter"
    
    -- Instructions box
    , translate 0 (-180) $ color (makeColorI 40 40 60 200) $ rectangleSolid 580 80
    , translate (-280) (-160) $ scale 0.11 0.11 $ color (makeColorI 200 220 255 255) $ text "Controls: WASD=Move | Space=Shoot | Arrows=Navigate | Enter=Confirm"
    , translate (-280) (-190) $ scale 0.11 0.11 $ color (makeColorI 200 220 255 255) $ text "Objective: Survive waves of enemies! Don't let 3 escape!"
    
    -- Mode selection
    , translate (-150) 80  $ scale 0.15 0.15 $ color (makeColorI 255 210 100 255) $ text "Select Mode:"
    , translate (-150) 50  $ selectable (selMode == Solo) "Solo - 1 Player Only (Hard)"
    , translate (-150) 20  $ selectable (selMode == CoopBot) "Coop+Bot - 1 Player + AI (Easy)"
    , translate (-150) (-10) $ selectable (selMode == Coop) "Coop - 2 Players vs Enemies"
    , translate (-150) (-40) $ selectable (selMode == PvP) "PvP - 2 Players Fight!"
    
    -- Player selection
    , translate (-150) (-75) $ scale 0.15 0.15 $ color (makeColorI 255 210 100 255) $ text "Select Player:"
    , translate (-150) (-105) $ selectable (selPlayer == Player1) "Player 1"
    , translate (-150) (-135) $ selectable (selPlayer == Player2) "Player 2"
    ]
    where
        selectable on label =
            let c = if on then makeColorI 80 255 150 255 else white
            in scale 0.13 0.13 $ color c $ text (prefix on ++ label)
        prefix True  = ">>> "
        prefix False = "    "

-- Hàm phụ trợ để tìm player
findPlayer :: PlayerID -> GameState -> Maybe Player
findPlayer pID gs = 
    let players = gamePlayer gs
        found = filter ((== pID) . playerID) players
    in case found of
        (p:_) -> Just p
        []    -> Nothing