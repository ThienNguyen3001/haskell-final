module Game.Logic (stepGame) where

import Data.List (nub, partition)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import Game.Data
import Game.Constants
import Game.AI
import Game.Physics

-- Core game logic
stepGame :: Float -> GameState -> (GameState, [Bullet])
stepGame deltaT gs = (finalGameState, newPlayerBullets ++ newEnemyBullets)
  where
    -- Ensure bot exists in CoopBot mode only, remove bot in Solo mode
    -- Bot gets the PlayerID that is not taken by human player
    playersWithBot = case gameMode gs of
      Solo    ->
        let humans = filter ((== Human) . playerType) (gamePlayer gs)
            pickOne =
              case humans of
                [] -> []
                hs ->
                  let mP1 = filter ((== Player1) . playerID) hs
                  in if not (null mP1) then [head mP1] else [head hs]
        in pickOne  -- Keep only one human in solo
      CoopBot ->
        let hasBot   = any (== Bot) (map playerType (gamePlayer gs))
            humanIDs = map playerID $ filter ((== Human) . playerType) (gamePlayer gs)
            botID    = if Player1 `elem` humanIDs then Player2 else Player1
            botPos   = if botID == Player1 then Position (-100) (-250) else Position 100 (-250)
        in if hasBot
             then gamePlayer gs
             else Player botID botPos 3 0 0 Idle False Bot 0.0 0.0 0 BotSpawn : gamePlayer gs
      _       -> filter ((== Human) . playerType) (gamePlayer gs)  -- Remove bots in Coop/PvP

    -- Let bot decide simple actions (use deltaT for shoot cooldown)
    -- Don't control dead bots waiting to respawn
    botControlledPlayers = map (\p -> 
      if playerType p == Bot
        then botStepFSM deltaT p (gameEnemies gs) (gameItems gs)
        else p
      ) playersWithBot

    -- Item spawn timer: spawn at random X near top and let them fall
    spawnTimer' = gameSpawnTimer gs - deltaT
    (itemsSpawned, nextSpawnTimer) =
      if spawnTimer' <= 0
        then let seed = gameEnemiesSpawned gs + gameLevel gs + length (gameItems gs)
                 xI = randX seed
                 yI = (screenHeight / 2) - 40
             in (Item (Position xI yI) 1 : gameItems gs, 5.0)
        else (gameItems gs, spawnTimer')
    -- make items fall
    itemsAfterSpawn = updateItems deltaT itemsSpawned

    -- Players update and bullets fired
    (updatedPlayers, newPlayerBullets) = updatePlayers (gameMode gs) deltaT botControlledPlayers

    -- Enemy spawn logic via timer - continuous spawning
    enemySpawnTimer' = gameEnemySpawnTimer gs - deltaT
    
    -- Disable enemy spawning entirely in PvP mode
    (enemiesAfterSpawn, nextEnemySpawnTimer, newSpawnCount) =
      if gameMode gs == PvP
         then ([], gameEnemySpawnTimer gs, 0)
         else if enemySpawnTimer' <= 0
           then let nSpawned = gameEnemiesSpawned gs
                    xSpawn = randX nSpawned
                    eType  = if (nSpawned `mod` 5) == 0 then BigEnemy else SmallEnemy
                    hp     = if eType == BigEnemy then (2 + gameLevel gs `div` 3) else 1
                    cd0    = if eType == BigEnemy then 0.5 else 1.0
                    newEnemy = Enemy (Position xSpawn 200) eType Idle Alive hp cd0
                in (newEnemy : gameEnemies gs, max 0.8 (2.0 - fromIntegral (gameLevel gs) * 0.05), 1)
           else (gameEnemies gs, enemySpawnTimer', 0)

    -- Enemies update and bullets fired, track escaped enemies
    (updatedEnemies, newEnemyBullets, escapedCount) =
      if gameMode gs == PvP
         then ([], [], 0)
         else updateEnemiesWithMode (gameMode gs) deltaT enemiesAfterSpawn

    -- Move existing bullets
    updatedOldBullets = updateBullets deltaT (gameBullets gs)

    -- Intermediate state before collisions
    -- Increase level gradually every 10 spawns
    letLevel = if (gameEnemiesSpawned gs `mod` 10) == 9 then gameLevel gs + 1 else gameLevel gs
    tempGameState = gs { gamePlayer = updatedPlayers
                       , gameEnemies = updatedEnemies
                       , gameBullets = updatedOldBullets
                       , gameItems = itemsAfterSpawn
                       , gameSpawnTimer = nextSpawnTimer
                       , gameEnemySpawnTimer = nextEnemySpawnTimer
                       , gameEnemiesSpawned = gameEnemiesSpawned gs + newSpawnCount
                       , gameLevel = letLevel
                       , gameEnemiesEscaped = gameEnemiesEscaped gs + escapedCount
                       }

    -- Resolve collisions and scoring
    finalGameState = handleCollisions tempGameState

-- Updates
updatePlayers :: GameMode -> Float -> [Player] -> ([Player], [Bullet])
updatePlayers mode deltaT players =
  let (ps, bs) = unzip (map (updatePlayer mode deltaT) players)
      -- Handle bot respawning
      respawnedPs = map (handleBotRespawn deltaT) ps
  in (respawnedPs, concat bs)

-- Handle bot respawn timer
handleBotRespawn :: Float -> Player -> Player
handleBotRespawn deltaT p
  | playerType p == Bot && playerLives p <= 0 && playerRespawnTimer p > 0 =
      let newTimer = playerRespawnTimer p - deltaT
   in if newTimer <= 0
     then -- Respawn bot with full health at spawn position (PvP not used for bots)
       let spawnPos = if playerID p == Player1 then Position (-100) (-250) else Position 100 (-250)
       in p { playerLives = 3
         , playerPos = spawnPos
         , playerRespawnTimer = 0.0
         , playerAction = Idle
         , playerWantsToShoot = False
         , playerDamageTaken = 0
         , playerBotState = BotSpawn }
    else p { playerRespawnTimer = newTimer, playerBotState = BotDead }
  | otherwise = p

updatePlayer :: GameMode -> Float -> Player -> (Player, [Bullet])
updatePlayer mode deltaT p =
  let (dx, dy) = case playerAction p of
        MoveUp    -> (0, playerSpeed * deltaT)
        MoveDown  -> (0, -playerSpeed * deltaT)
        MoveLeft  -> (-playerSpeed * deltaT, 0)
        MoveRight -> (playerSpeed * deltaT, 0)
        _         -> (0, 0)
      Position x y = playerPos p
      newPos = Position (clamp (x + dx) (-screenWidth/2) (screenWidth/2))
                        (clamp (y + dy) (-screenHeight/2) (screenHeight/2))
      newCooldown = max 0 (playerShootCooldown p - deltaT)
      canShoot = newCooldown <= 0
      bulletDirVal = if mode == PvP && playerID p == Player2 then (-1) else 1
      bulletSpawnY = if bulletDirVal == 1 then y + entitySize else y - entitySize
      newBullet = if playerWantsToShoot p && canShoot
                    then [Bullet (Position x bulletSpawnY) PlayerOwned (Just $ playerID p) bulletDirVal]
                    else []
      -- Bots fire a bit slower to avoid OP
      botFireCd = 0.35 :: Float
      humanFireCd = 0.2 :: Float
      fireReset = if playerType p == Bot then botFireCd else humanFireCd
      finalCooldown = if playerWantsToShoot p && canShoot then fireReset else newCooldown
      p' = p { playerPos = newPos
             , playerWantsToShoot = False
             , playerShootCooldown = finalCooldown }
  in (p', newBullet)

-- Update enemies with mode-based speed
-- Update enemies with mode-based speed and track escaped
updateEnemiesWithMode :: GameMode -> Float -> [Enemy] -> ([Enemy], [Bullet], Int)
updateEnemiesWithMode mode deltaT enemies =
  let enemySpeed = getEnemySpeed mode
      process e =
        let (Position x y) = enemyPos e
            newPos = Position x (y - enemySpeed * deltaT)
            cd'    = enemyFireCd e - deltaT
        in if cd' <= 0
              then ( e { enemyPos = newPos
                        , enemyFireCd = resetCd (enemyType e) }
                   , [Bullet (Position x (y - entitySize)) EnemyOwned Nothing (-1)] )
              else ( e { enemyPos = newPos, enemyFireCd = cd' }
                   , [] )
      (es, bs) = unzip (map process enemies)
      -- Separate escaped from in-bounds
      (esInBounds, esEscaped) = partition (\e -> let Position _ y = enemyPos e 
                                                 in y > (-screenHeight/2 - entitySize)) es
  in (esInBounds, concat bs, length esEscaped)

resetCd :: EnemyType -> Float
resetCd SmallEnemy = 1.2
resetCd BigEnemy   = 0.7

updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets deltaT = mapMaybe $ \b ->
  let (Position x y) = bulletPos b
      dy = case bulletOwner b of
             PlayerOwned -> bulletSpeed * deltaT * bulletDir b
             EnemyOwned  -> -bulletSpeed * deltaT
      newPos = Position x (y + dy)
  in if abs (posY newPos) > screenHeight / 2
       then Nothing
       else Just (b { bulletPos = newPos })

-- Update items: fall down and cull off-screen
updateItems :: Float -> [Item] -> [Item]
updateItems dt = mapMaybe $ \it ->
  let Position x y = itemPos it
      y' = y - itemSpeed * dt
      newPos = Position x y'
  in if y' < (-screenHeight / 2 - entitySize)
        then Nothing
        else Just (it { itemPos = newPos })

-- Collisions and scoring
handleCollisions :: GameState -> GameState
handleCollisions gs = gs { gamePlayer = scoredPlayers
                         , gameEnemies = updatedEnemies
                         , gameBullets = remainingPlayerBullets ++ remainingEnemyBullets
                         , gameItems   = remainingItems ++ enemyDrops (gameEnemiesSpawned gs) hitEnemies }
  where
    players = gamePlayer gs
    enemies = gameEnemies gs
    bullets = gameBullets gs
    items   = gameItems gs
    mode    = gameMode gs

    playerBullets = filter ((== PlayerOwned) . bulletOwner) bullets
    enemyBullets  = filter ((== EnemyOwned) . bulletOwner) bullets

    (hitByPvP, remainingPvPBullets, survivingPlayers) =
      if mode == PvP
        then checkPvPCollisions playerBullets players
        else ([], playerBullets, players)

    (hitEnemies, hitPlayerBullets, remainingPlayerBullets) =
      checkBulletEnemyCollisions remainingPvPBullets enemies
    (hitPlayers, remainingEnemyBullets) =
      checkBulletPlayerCollisions enemyBullets survivingPlayers

    -- Player vs Enemy touch (sum of half extents on both axes; inclusive bounds)
    touchedPlayers =
      [ p
      | p <- survivingPlayers
      , any (\e -> collidesAABB (playerPos p) (enemyPos e)
                          (playerHalf + enemyHalf)
                          (playerHalf + enemyHalf))
             enemies
      ]

    updatedEnemies = filter (`notElem` hitEnemies) enemies

    hitPlayerIDs = nub (map playerID hitPlayers ++ map playerID touchedPlayers ++ map playerID hitByPvP)

    updatedPlayers = map updateOne survivingPlayers
    updateOne p =
      if playerID p `elem` hitPlayerIDs
        then
          let newLives  = playerLives p - 1
              newDeaths = playerDeaths p + (if newLives <= 0 then 1 else 0)
              newDmg    = playerDamageTaken p + 1
          in if newLives <= 0 && playerType p == Bot
               then let spawnPos = if playerID p == Player1 then Position (-100) (-250) else Position 100 (-250)
                    in p { playerLives = 3
                         , playerDeaths = newDeaths
                         , playerRespawnTimer = 0.0
                         , playerPos = spawnPos
                         , playerAction = Idle
                         , playerWantsToShoot = False
                         , playerBotState = BotSpawn
                         , playerDamageTaken = 0 }
               else p { playerLives = newLives
                       , playerDeaths = newDeaths
                       , playerDamageTaken = newDmg }
        else p

    alivePlayers = filter keepPlayer updatedPlayers
    keepPlayer p = case playerType p of
                     Human -> True
                     Bot   -> playerLives p > 0 || playerRespawnTimer p > 0

    (remainingItems, playersAfterItems) = foldr collectItem ([], alivePlayers) items
    collectItem it (accItems, accPlayers) =
      let collidedPlayers = any (\p -> collidesAABB (playerPos p) (itemPos it)
                                          (playerHalf + itemHalf)
                                          (playerHalf + itemHalf)) accPlayers
      in if collidedPlayers
           then ( accItems
                , map (\p -> if collidesAABB (playerPos p) (itemPos it)
                                       (playerHalf + itemHalf)
                                       (playerHalf + itemHalf)
                               then let healed = itemHeal it
                                        newLives = playerLives p + healed
                                        newDmg = max 0 (playerDamageTaken p - healed)
                                     in p { playerLives = newLives, playerDamageTaken = newDmg }
                               else p) accPlayers )
           else (it:accItems, accPlayers)

    shooterHits = foldr countHit Map.empty hitPlayerBullets
    countHit b acc = case bulletShooter b of
                       Just pid -> Map.insertWith (+) pid 1 acc
                       Nothing  -> acc

    scoredPlayers = map addScore playersAfterItems
    addScore p = p { playerScore = playerScore p + Map.findWithDefault 0 (playerID p) shooterHits }

-- Drop items at enemy death positions with 30% chance, deterministic by spawn count
enemyDrops :: Int -> [Enemy] -> [Item]
enemyDrops base hits =
  let tagged = zip [base..] hits
      keep i = (i * 1664525 + 1013904223) `mod` 10 < 3
      toItem (_,e) = Item (enemyPos e) 1
  in map toItem (filter (keep . fst) tagged)

-- Utils
clamp :: (Ord a) => a -> a -> a -> a
clamp val minVal maxVal = max minVal (min maxVal val)

-- Simple deterministic pseudo-random X based on spawn index
randX :: Int -> Float
randX n =
  let f = fromIntegral ((n * 1103515245 + 12345) `mod` 2147483647) / 2147483647.0
      range = (screenWidth / 2 - 50)
  in (-range) + f * (2 * range)