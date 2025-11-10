module Game.Constants where

import Game.Data

-- Từ Server.hs
playerSpeed, bulletSpeed, itemSpeed, entitySize :: Float
playerSpeed = 200.0
bulletSpeed = 400.0
itemSpeed   = 60.0
entitySize  = 20.0

playerHalf, enemyHalf, bulletHalf, itemHalf :: Float
playerHalf = entitySize
enemyHalf  = entitySize
bulletHalf = entitySize * 0.4
itemHalf   = entitySize

getEnemySpeed :: GameMode -> Float
getEnemySpeed Solo    = 100.0
getEnemySpeed CoopBot = 50.0
getEnemySpeed Coop    = 100.0
getEnemySpeed PvP     = 75.0

screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

initialGameState :: GameState
initialGameState = GameState [] [] [] 0 0 0 0 0 5.0 2.0 [] False Coop 0

-- Từ GameRender.hs
margin :: Float
margin = 14

scalePlayer, scaleEnemy, scaleBullet, scaleItem :: Float
scalePlayer = 0.05
scaleEnemy  = 0.05
scaleBullet = 0.05
scaleItem   = 0.05