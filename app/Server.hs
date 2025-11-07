module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM_, void, when)
import qualified Data.Map as Map
import Data.List (nub, partition)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Data.Maybe (mapMaybe)

import GameData
import GameMes

-- Server state: game state + client address map
data ServerState = ServerState
  { gameState :: TVar GameState
  , clients   :: TVar (Map.Map PlayerID SockAddr)
  }

playerSpeed, bulletSpeed, itemSpeed, entitySize :: Float
playerSpeed = 200.0
bulletSpeed = 400.0
itemSpeed   = 60.0
entitySize  = 20.0

-- Enemy speed based on mode
getEnemySpeed :: GameMode -> Float
getEnemySpeed Solo    = 100.0  -- Normal speed for solo (harder)
getEnemySpeed CoopBot = 50.0   -- Slower for coop with bot (easier)
getEnemySpeed Coop    = 100.0  -- Normal speed for 2 players
getEnemySpeed PvP     = 75.0   -- Slightly slower in PvP since players fight each other

screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

-- Initial game state
initialGameState :: GameState
initialGameState = GameState [] [] [] 0 0 0 0 0 5.0 2.0 [] False Coop 0

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting server on port 8080..."
  addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram })) Nothing (Just "8080")
  case addrInfos of
    (serverAddr:_) -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      bind sock (addrAddress serverAddr)
      serverMain sock
    [] -> error "No address info for server"

serverMain :: Socket -> IO ()
serverMain sock = do
  gsTVar <- newTVarIO initialGameState
  clientsTVar <- newTVarIO Map.empty
  let state = ServerState gsTVar clientsTVar

  _ <- forkIO $ receiverLoop sock state
  _ <- forkIO $ gameLoop sock state

  forever $ threadDelay (10 * 1000 * 1000)

-- Receiver loop
receiverLoop :: Socket -> ServerState -> IO ()
receiverLoop sock state = forever $ do
  (byteString, clientAddr) <- recvFrom sock 1024
  let clientMsg = decode (LBS.fromStrict byteString) :: ClientMessage
  case clientMsg of
    JoinGame pID            -> handleJoinGame sock state pID clientAddr
    PlayerAction pID action -> handlePlayerAction state pID action
    PlayerQuit pID          -> handlePlayerQuit state pID
    SetMode mode            -> atomically $ writeTVar (gameState state) (initialGameState { gameMode = mode })
    _                       -> return ()

-- Game loop (~60 FPS)
gameLoop :: Socket -> ServerState -> IO ()
gameLoop sock state = forever $ do
  let deltaT = 0.016
  
  -- Check if we need to reset (no players connected)
  clientMap <- atomically $ readTVar (clients state)
  when (Map.null clientMap) $ atomically $ writeTVar (gameState state) initialGameState
  
  updatedGameState <- atomically $ do
    gs <- readTVar (gameState state)
    
    -- Check game over conditions first (only human players matter)
    let humans = filter ((== Human) . playerType) (gamePlayer gs)
        allHumansDead = not (null humans) && all ((<= 0) . playerLives) humans
        anyHumanDead = not (null humans) && any ((<= 0) . playerLives) humans
        tooManyEscaped = gameEnemiesEscaped gs >= 3
        
        -- Bot deaths don't cause game over, only human deaths
        isGameOver = case gameMode gs of
          PvP -> anyHumanDead  -- In PvP, any human death ends game
          _   -> allHumansDead || tooManyEscaped  -- In Coop/Solo, all humans must die OR too many escaped
    
    -- If game is over, don't update - just return current state
    if isGameOver
      then return gs
      else do
        -- Check if game can start based on mode requirements
        let humanCount = length $ filter ((== Human) . playerType) (gamePlayer gs)
            canPlay = case gameMode gs of
                        Solo    -> humanCount >= 1  -- Solo: 1 player only
                        CoopBot -> humanCount >= 1  -- CoopBot: 1 player + bot
                        Coop    -> humanCount >= 2  -- Coop: 2 players required
                        PvP     -> humanCount == 2  -- PvP: exactly 2 players
        
        if not canPlay
          then return gs  -- Don't update game if requirements not met
          else do
            let (tickedGs, bullets) = runGameLogic deltaT gs
            let finalGs = tickedGs { gameBullets = gameBullets tickedGs ++ bullets }
            writeTVar (gameState state) finalGs
            return finalGs

  let humans = filter ((== Human) . playerType) (gamePlayer updatedGameState)
      allHumansDead = not (null humans) && all ((<= 0) . playerLives) humans
      anyHumanDead = not (null humans) && any ((<= 0) . playerLives) humans
      tooManyEscaped = gameEnemiesEscaped updatedGameState >= 3
      
      -- Game over conditions depend on mode (bot deaths don't count)
      isGameOver = case gameMode updatedGameState of
        PvP -> anyHumanDead  -- In PvP, any human death = game over (the other wins)
        _   -> allHumansDead || tooManyEscaped  -- In other modes, all humans must die OR too many escaped
      
      serverMsg 
        | isGameOver = GameOver False
        | otherwise = UpdateGame updatedGameState
      
      encodedMsg = LBS.toStrict $ encode serverMsg
  
  forM_ (Map.elems clientMap) $ \addr -> sendTo sock encodedMsg addr
  threadDelay (floor (deltaT * 1000 * 1000))

-- Join/leave/actions
handleJoinGame :: Socket -> ServerState -> PlayerID -> SockAddr -> IO ()
handleJoinGame sock state pID addr = do
  putStrLn $ "Player " ++ show pID ++ " joined from " ++ show addr
  let startPos = if pID == Player1 then Position (-100) (-250) else Position 100 (-250)
  let newPlayer = Player pID startPos 3 0 0 Idle False Human 0.0 0.0
  (welcomeState, newCount) <- atomically $ do
    clientsMap <- readTVar (clients state)
    let newClientsMap = Map.insert pID addr clientsMap
    writeTVar (clients state) newClientsMap

    gs <- readTVar (gameState state)
    let otherPlayers = filter ((/= pID) . playerID) (gamePlayer gs)
    let finalPlayers = newPlayer : otherPlayers
    let newGs = gs { gamePlayer = finalPlayers }
    writeTVar (gameState state) newGs
    return (newGs, Map.size newClientsMap)

  putStrLn $ "Total clients: " ++ show newCount
  void $ sendTo sock (LBS.toStrict $ encode (Welcome pID welcomeState)) addr

handlePlayerQuit :: ServerState -> PlayerID -> IO ()
handlePlayerQuit state pID = atomically $ do
  clientsMap <- readTVar (clients state)
  writeTVar (clients state) (Map.delete pID clientsMap)
  gs <- readTVar (gameState state)
  writeTVar (gameState state) (gs { gamePlayer = filter ((/= pID) . playerID) (gamePlayer gs) })

handlePlayerAction :: ServerState -> PlayerID -> Action -> IO ()
handlePlayerAction state pID action = atomically $ do
  gs <- readTVar (gameState state)
  let upd p = if playerID p == pID
              then case action of
                     Shoot -> p { playerWantsToShoot = True }
                     _     -> p { playerAction = action }
              else p
  writeTVar (gameState state) (gs { gamePlayer = map upd (gamePlayer gs) })

-- Core game logic
runGameLogic :: Float -> GameState -> (GameState, [Bullet])
runGameLogic deltaT gs = (finalGameState, newPlayerBullets ++ newEnemyBullets)
  where
    -- Ensure bot exists in CoopBot mode only, remove bot in Solo mode
    -- Bot gets the PlayerID that is not taken by human player
    playersWithBot = case gameMode gs of
      Solo    -> filter ((== Human) . playerType) (gamePlayer gs)  -- Remove bot in solo
      CoopBot -> let hasBot = any (== Bot) (map playerType (gamePlayer gs))
                     humanIDs = map playerID $ filter ((== Human) . playerType) (gamePlayer gs)
                     -- Assign bot to Player1 if no human has it, otherwise Player2
                     botID = if Player1 `elem` humanIDs then Player2 else Player1
                     botPos = if botID == Player1 then Position (-100) (-250) else Position 100 (-250)
                 in if hasBot 
                      then gamePlayer gs
                      else Player botID botPos 3 0 0 Idle False Bot 0.0 0.0 : gamePlayer gs
      _       -> filter ((== Human) . playerType) (gamePlayer gs)  -- Remove bots in Coop/PvP

    -- Let bot decide simple actions (use deltaT for shoot cooldown)
    -- Don't control dead bots waiting to respawn
    botControlledPlayers = map (\p -> 
      if playerType p == Bot && playerLives p > 0
        then botLogic deltaT p (gameEnemies gs) (gameItems gs)
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
    (updatedPlayers, newPlayerBullets) = updatePlayers deltaT botControlledPlayers

    -- Enemy spawn logic via timer - continuous spawning
    enemySpawnTimer' = gameEnemySpawnTimer gs - deltaT
    
    (enemiesAfterSpawn, nextEnemySpawnTimer, newSpawnCount) =
      if enemySpawnTimer' <= 0
         then let nSpawned = gameEnemiesSpawned gs
                  xSpawn = randX nSpawned
                  -- Mix of enemy types based on level
                  eType  = if (nSpawned `mod` 5) == 0 then BigEnemy else SmallEnemy
                  hp     = if eType == BigEnemy then (2 + gameLevel gs `div` 3) else 1
                  cd0    = if eType == BigEnemy then 0.5 else 1.0
                  newEnemy = Enemy (Position xSpawn 200) eType Idle Alive hp cd0
              in (newEnemy : gameEnemies gs, max 0.8 (2.0 - fromIntegral (gameLevel gs) * 0.05), 1)
         else (gameEnemies gs, enemySpawnTimer', 0)

    -- Enemies update and bullets fired, track escaped enemies
    (updatedEnemies, newEnemyBullets, escapedCount) = updateEnemiesWithMode (gameMode gs) deltaT enemiesAfterSpawn

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
updatePlayers :: Float -> [Player] -> ([Player], [Bullet])
updatePlayers deltaT players =
  let (ps, bs) = unzip (map (updatePlayer deltaT) players)
      -- Handle bot respawning
      respawnedPs = map (handleBotRespawn deltaT) ps
  in (respawnedPs, concat bs)

-- Handle bot respawn timer
handleBotRespawn :: Float -> Player -> Player
handleBotRespawn deltaT p
  | playerType p == Bot && playerLives p <= 0 && playerRespawnTimer p > 0 =
      let newTimer = playerRespawnTimer p - deltaT
      in if newTimer <= 0
         then -- Respawn bot with full health at spawn position
              let spawnPos = if playerID p == Player1 then Position (-100) (-250) else Position 100 (-250)
              in p { playerLives = 3
                   , playerPos = spawnPos
                   , playerRespawnTimer = 0.0
                   , playerAction = Idle
                   , playerWantsToShoot = False }
         else p { playerRespawnTimer = newTimer }
  | otherwise = p

updatePlayer :: Float -> Player -> (Player, [Bullet])
updatePlayer deltaT p =
  let (dx, dy) = case playerAction p of
        MoveUp    -> (0, playerSpeed * deltaT)
        MoveDown  -> (0, -playerSpeed * deltaT)
        MoveLeft  -> (-playerSpeed * deltaT, 0)
        MoveRight -> (playerSpeed * deltaT, 0)
        _         -> (0, 0)
      (Position x y) = playerPos p
      newPos = Position (clamp (x + dx) (-screenWidth/2) (screenWidth/2))
                        (clamp (y + dy) (-screenHeight/2) (screenHeight/2))
      
      -- Update shoot cooldown
      newCooldown = max 0 (playerShootCooldown p - deltaT)
      canShoot = newCooldown <= 0
      
      newBullet = if playerWantsToShoot p && canShoot
                  then [Bullet (Position x (y + entitySize)) PlayerOwned (Just $ playerID p)]
                  else []
      
      -- Reset cooldown if shot fired (0.2 seconds = 5 shots per second max)
      finalCooldown = if playerWantsToShoot p && canShoot then 0.2 else newCooldown
      
  in (p { playerPos = newPos, playerWantsToShoot = False, playerShootCooldown = finalCooldown }, newBullet)

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
                   , [Bullet (Position x (y - entitySize)) EnemyOwned Nothing] )
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
             PlayerOwned -> bulletSpeed * deltaT
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
handleCollisions gs =
  let players = gamePlayer gs
      enemies = gameEnemies gs
      bullets = gameBullets gs
      items   = gameItems gs
      mode    = gameMode gs

      playerBullets = filter ((== PlayerOwned) . bulletOwner) bullets
      enemyBullets  = filter ((== EnemyOwned) . bulletOwner) bullets

      -- In PvP mode, check for player-vs-player bullet hits
      (hitByPvP, remainingPvPBullets, survivingPlayers) =
        if mode == PvP
          then checkPvPCollisions playerBullets players
          else ([], playerBullets, players)

      (hitEnemies, hitPlayerBullets, remainingPlayerBullets) =
        checkBulletEnemyCollisions remainingPvPBullets enemies
      (hitPlayers, remainingEnemyBullets) =
        checkBulletPlayerCollisions enemyBullets survivingPlayers

      -- Players touching enemies also lose 1 life (once per tick)
      touchedPlayers = [ p | p <- survivingPlayers
                           , any (\e -> isColliding (playerPos p) (enemyPos e) entitySize) enemies ]

      updatedEnemies = filter (`notElem` hitEnemies) enemies

      hitPlayerIDs = nub (map playerID hitPlayers ++ map playerID touchedPlayers ++ map playerID hitByPvP)
      
      -- Update players: reduce lives if hit, set respawn timer for dead bots
      updatedPlayers = map (\p -> 
        if playerID p `elem` hitPlayerIDs
          then let newLives = playerLives p - 1
                   newDeaths = playerDeaths p + (if newLives <= 0 then 1 else 0)
               in if newLives <= 0 && playerType p == Bot
                  then p { playerLives = 0, playerDeaths = newDeaths, playerRespawnTimer = 3.0 }  -- Bot respawns in 3s
                  else p { playerLives = newLives, playerDeaths = newDeaths }
          else p
        ) survivingPlayers
      
      -- Keep all players (including dead bots waiting to respawn)
      -- Only remove human players with 0 lives
      alivePlayers = filter (\p -> playerLives p > 0 || (playerType p == Bot && playerRespawnTimer p > 0)) updatedPlayers

      (remainingItems, playersAfterItems) = foldr
        (\it (accItems, accPlayers) ->
          let collided = any (\p -> isColliding (playerPos p) (itemPos it) entitySize) accPlayers
          in if collided
               then ( accItems
                    , map (\p -> if isColliding (playerPos p) (itemPos it) entitySize
                                   then p { playerLives = playerLives p + itemHeal it }
                                   else p) accPlayers)
               else (it:accItems, accPlayers))
        ([], alivePlayers)
        items

      shooterHits = foldr (\b acc -> case bulletShooter b of
                            Just pid -> Map.insertWith (+) pid 1 acc
                            Nothing  -> acc)
                      Map.empty
                      hitPlayerBullets

      scoredPlayers = map (\p -> p { playerScore = playerScore p
                                      + Map.findWithDefault 0 (playerID p) shooterHits })
                       playersAfterItems

  in gs { gamePlayer = scoredPlayers
        , gameEnemies = updatedEnemies
        , gameBullets = remainingPlayerBullets ++ remainingEnemyBullets
        , gameItems   = remainingItems ++ enemyDrops (gameEnemiesSpawned gs) hitEnemies }

-- Drop items at enemy death positions with 30% chance, deterministic by spawn count
enemyDrops :: Int -> [Enemy] -> [Item]
enemyDrops base hits =
  let tagged = zip [base..] hits
      keep i = (i * 1664525 + 1013904223) `mod` 10 < 3
      toItem (_,e) = Item (enemyPos e) 1
  in map toItem (filter (keep . fst) tagged)

-- Collision helpers
checkPvPCollisions :: [Bullet] -> [Player] -> ([Player], [Bullet], [Player])
checkPvPCollisions bullets players =
  let collisions = [(b, p) | b <- bullets
                           , p <- players
                           , isColliding (bulletPos b) (playerPos p) entitySize
                           , case bulletShooter b of
                               Just shooterId -> shooterId /= playerID p  -- Don't hit yourself
                               Nothing -> False]
      hitPlayers = map snd collisions
      hitBullets = map fst collisions
      remainingBullets = filter (`notElem` hitBullets) bullets
  in (hitPlayers, remainingBullets, players)

checkBulletEnemyCollisions :: [Bullet] -> [Enemy] -> ([Enemy], [Bullet], [Bullet])
checkBulletEnemyCollisions bullets enemies =
  let collisions = [(b, e) | b <- bullets, e <- enemies, isColliding (bulletPos b) (enemyPos e) entitySize]
      hitEnemies = map snd collisions
      hitBullets = map fst collisions
      remainingBullets = filter (`notElem` hitBullets) bullets
  in (hitEnemies, hitBullets, remainingBullets)

checkBulletPlayerCollisions :: [Bullet] -> [Player] -> ([Player], [Bullet])
checkBulletPlayerCollisions bullets players =
  let collisions = [(b, p) | b <- bullets, p <- players, isColliding (bulletPos b) (playerPos p) entitySize]
      hitPlayers = map snd collisions
      hitBullets = map fst collisions
      remainingBullets = filter (`notElem` hitBullets) bullets
  in (hitPlayers, remainingBullets)

-- Utils
isColliding :: Position -> Position -> Float -> Bool
isColliding (Position x1 y1) (Position x2 y2) size =
  abs (x1 - x2) < size && abs (y1 - y2) < size

clamp :: (Ord a) => a -> a -> a -> a
clamp val minVal maxVal = max minVal (min maxVal val)

-- Advanced Bot AI - with shooting cooldown, item collection, and better enemy tracking
botLogic :: Float -> Player -> [Enemy] -> [Item] -> Player
botLogic deltaT p enemies items =
  let cd = playerShootCooldown p
      newCd = max 0 (cd - deltaT)
      
      -- Find nearest item if health is low
      lowHealth = playerLives p <= 2
      nearbyItems = filter (\it -> distance (playerPos p) (itemPos it) < 200) items
      
      -- Decide if we should prioritize item collection
      shouldGetItem = lowHealth && not (null nearbyItems)
      
      closestItem = if null nearbyItems then Nothing
                    else let itemsWithDist = map (\it -> (it, distance (playerPos p) (itemPos it))) nearbyItems
                             (item, _) = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) itemsWithDist
                         in Just item
      
  in if shouldGetItem
       then case closestItem of
              Nothing -> idleBot p newCd
              Just item -> moveTowardsItem p item newCd
       else case enemies of
              [] -> idleBot p newCd
              _  -> fightEnemies p enemies newCd

-- Helper: Bot idles
idleBot :: Player -> Float -> Player
idleBot p cd = p { playerAction = Idle, playerWantsToShoot = False, playerShootCooldown = cd }

-- Helper: Bot moves towards item
moveTowardsItem :: Player -> Item -> Float -> Player
moveTowardsItem p item cd =
  let (Position px py) = playerPos p
      (Position ix iy) = itemPos item
      dx = ix - px
      dy = iy - py
      
      -- Move towards item
      action
        | abs dx < 10 && abs dy < 10 = Idle  -- Close enough
        | abs dx > abs dy = if dx < 0 then MoveLeft else MoveRight
        | dy < 0 = MoveDown
        | otherwise = MoveUp
        
  in p { playerAction = action, playerWantsToShoot = False, playerShootCooldown = cd }

-- Helper: Bot fights enemies
fightEnemies :: Player -> [Enemy] -> Float -> Player
fightEnemies p enemies cd =
  let (Position px _py) = playerPos p
      enemiesWithDist = map (\e -> (e, distance (playerPos p) (enemyPos e))) enemies
      (closestEnemy, _) = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) enemiesWithDist
      
      (Position ex ey) = enemyPos closestEnemy
      dx = ex - px
      
      -- Calculate if we should shoot
      horizontallyAligned = abs dx < 80
      enemyAbove = ey > posY (playerPos p) - 30  -- Enemy above or near same level
      canShoot = cd <= 0
      shouldShoot = horizontallyAligned && enemyAbove && canShoot
      
      -- Movement logic - position under enemy
      action 
        | abs dx < 15 = Idle  -- Well positioned
        | dx < -10 = MoveLeft
        | dx > 10 = MoveRight
        | otherwise = Idle
        
  in p { playerAction = action, playerWantsToShoot = shouldShoot, playerShootCooldown = cd }

-- Helper function to calculate distance between two positions
distance :: Position -> Position -> Float
distance (Position x1 y1) (Position x2 y2) = 
    sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs
minimumBy _ [] = error "minimumBy: empty list"

-- Simple deterministic pseudo-random X based on spawn index
randX :: Int -> Float
randX n =
  let f = fromIntegral ((n * 1103515245 + 12345) `mod` 2147483647) / 2147483647.0
      range = (screenWidth / 2 - 50)
  in (-range) + f * (2 * range)