module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM_, void)
import qualified Data.Map as Map
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

-- Constants
playerSpeed, bulletSpeed, enemySpeed, entitySize :: Float
playerSpeed = 200.0
bulletSpeed = 400.0
enemySpeed  = 100.0
entitySize  = 20.0

screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

-- Initial game state
initialGameState :: GameState
initialGameState = GameState [] [] [] 0 0 0 0 0 5.0 2.0 [] False Coop

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
    SetMode mode            -> atomically $ modifyTVar' (gameState state) (\gs -> gs { gameMode = mode })
    _                       -> return ()

-- Game loop (~60 FPS)
gameLoop :: Socket -> ServerState -> IO ()
gameLoop sock state = forever $ do
  let deltaT = 0.016
  updatedGameState <- atomically $ do
    gs <- readTVar (gameState state)
    let (tickedGs, bullets) = runGameLogic deltaT gs
    let finalGs = tickedGs { gameBullets = gameBullets tickedGs ++ bullets }
    writeTVar (gameState state) finalGs
    return finalGs

  clientMap <- atomically $ readTVar (clients state)
  let encodedMsg = LBS.toStrict $ encode (UpdateGame updatedGameState)
  forM_ (Map.elems clientMap) $ \addr -> sendTo sock encodedMsg addr
  threadDelay (floor (deltaT * 1000 * 1000))

-- Join/leave/actions
handleJoinGame :: Socket -> ServerState -> PlayerID -> SockAddr -> IO ()
handleJoinGame sock state pID addr = do
  putStrLn $ "Player " ++ show pID ++ " joined from " ++ show addr
  let startPos = if pID == Player1 then Position (-100) (-250) else Position 100 (-250)
  let newPlayer = Player pID startPos 3 0 0 Idle False Human
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
runGameLogic deltaT gs =
  let
    -- Ensure bot exists in Solo mode
    playersWithBot = case gameMode gs of
      Solo -> let hasBot = any (== Bot) (map playerType (gamePlayer gs))
              in if hasBot then gamePlayer gs
                 else Player Player2 (Position 100 (-250)) 3 0 0 Idle False Bot : gamePlayer gs
      _    -> gamePlayer gs

    -- Let bot decide simple actions
    botControlledPlayers = map (\p -> if playerType p == Bot then botLogic p (gameEnemies gs) else p) playersWithBot

    -- Item spawn timer
    spawnTimer' = gameSpawnTimer gs - deltaT
    (itemsAfterSpawn, nextSpawnTimer) =
      if spawnTimer' <= 0
        then (Item (Position 0 0) 1 : gameItems gs, 5.0)
        else (gameItems gs, spawnTimer')

    -- Players update and bullets fired
    (updatedPlayers, newPlayerBullets) = updatePlayers deltaT botControlledPlayers

    -- Enemy spawn logic via timer
    enemySpawnTimer' = gameEnemySpawnTimer gs - deltaT
    (enemiesAfterSpawn, nextEnemySpawnTimer) =
      if enemySpawnTimer' <= 0
         then let nSpawned = gameEnemiesSpawned gs
                  xSpawn = randX nSpawned
                  eType  = if (nSpawned `mod` 5) == 4 then BigEnemy else SmallEnemy
                  hp     = if eType == BigEnemy then 3 else 1
                  cd0    = if eType == BigEnemy then 0.5 else 1.0
                  newEnemy = Enemy (Position xSpawn 200) eType Idle Alive hp cd0
              in (newEnemy : gameEnemies gs, max 0.8 (3.0 - fromIntegral (gameLevel gs) * 0.2))
         else (gameEnemies gs, enemySpawnTimer')

    -- Enemies update and bullets fired
    (updatedEnemies, newEnemyBullets) = updateEnemies deltaT enemiesAfterSpawn

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
             , gameEnemiesSpawned = gameEnemiesSpawned gs + (if enemySpawnTimer' <= 0 then 1 else 0)
             , gameLevel = letLevel }

    -- Resolve collisions and scoring
    finalGameState = handleCollisions tempGameState
    allNewBullets = newPlayerBullets ++ newEnemyBullets
  in (finalGameState, allNewBullets)

-- Updates
updatePlayers :: Float -> [Player] -> ([Player], [Bullet])
updatePlayers deltaT players =
  let (ps, bs) = unzip (map (updatePlayer deltaT) players)
  in (ps, concat bs)

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
      newBullet = if playerWantsToShoot p
                  then [Bullet (Position x (y + entitySize)) PlayerOwned (Just $ playerID p)]
                  else []
  in (p { playerPos = newPos, playerWantsToShoot = False }, newBullet)

updateEnemies :: Float -> [Enemy] -> ([Enemy], [Bullet])
updateEnemies deltaT enemies =
  let process e =
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
      esInBounds = filter (\e -> let Position _ y = enemyPos e in y > (-screenHeight/2 - entitySize)) es
  in (esInBounds, concat bs)

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

-- Collisions and scoring
handleCollisions :: GameState -> GameState
handleCollisions gs =
  let players = gamePlayer gs
      enemies = gameEnemies gs
      bullets = gameBullets gs
      items   = gameItems gs

      playerBullets = filter ((== PlayerOwned) . bulletOwner) bullets
      enemyBullets  = filter ((== EnemyOwned) . bulletOwner) bullets

      (hitEnemies, hitPlayerBullets, remainingPlayerBullets) =
        checkBulletEnemyCollisions playerBullets enemies
      (hitPlayers, remainingEnemyBullets) =
        checkBulletPlayerCollisions enemyBullets players

      updatedEnemies = filter (`notElem` hitEnemies) enemies

      hitPlayerIDs = map playerID hitPlayers
      updatedPlayers = map (\p -> if playerID p `elem` hitPlayerIDs
                                    then p { playerLives = playerLives p - 1 }
                                    else p) players
      alivePlayers = filter ((> 0) . playerLives) updatedPlayers

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
      toItem (i,e) = Item (enemyPos e) 1
  in map toItem (filter (keep . fst) tagged)

-- Collision helpers
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

-- Very simple bot AI
botLogic :: Player -> [Enemy] -> Player
botLogic p enemies = case enemies of
  []    -> p { playerAction = Idle }
  (e:_) -> let (Position ex _) = enemyPos e
               (Position px _) = playerPos p
               dx = ex - px
               action = if abs dx < 10 then Shoot else if dx < 0 then MoveLeft else MoveRight
           in p { playerAction = action, playerWantsToShoot = (action == Shoot) }

-- Simple deterministic pseudo-random X based on spawn index
randX :: Int -> Float
randX n =
  let f = fromIntegral ((n * 1103515245 + 12345) `mod` 2147483647) / 2147483647.0
      range = (screenWidth / 2 - 50)
  in (-range) + f * (2 * range)