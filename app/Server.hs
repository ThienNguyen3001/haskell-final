module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM_, void, when)
import qualified Data.Map as Map
import Data.List (partition) -- Bỏ 'nub' nếu không dùng
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Data.Maybe (mapMaybe, maybeToList)

import Game.Data
import Game.Messaging
import Game.Constants -- Import hằng số
import Game.Logic     -- Import logic thuần túy

-- Server state: game state + client address map
data ServerState = ServerState
  { gameState :: TVar GameState
  , clients   :: TVar (Map.Map PlayerID SockAddr)
  }

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
    SetMode mode            -> do
      atomically $ handleSetModeFromAddr state mode clientAddr
      when (mode == Solo || mode == CoopBot) $ do
        -- Offline modes: keep only one client mapped to Player1
        mWelcome <- atomically $ do
          cs <- readTVar (clients state)
          gs <- readTVar (gameState state)
          let anyAddr = if Map.null cs then Nothing else Just (snd (head (Map.toList cs)))
              cs' = case anyAddr of
                      Just a  -> Map.singleton Player1 a
                      Nothing -> Map.empty
              humans = filter ((== Human) . playerType) (gamePlayer gs)
              pid = if null humans then Player1 else playerID (head humans)
          writeTVar (clients state) cs'
          return (fmap (\a -> (pid, gs, a)) anyAddr)
        forM_ (maybeToList mWelcome) $ \(pid, gsSnap, a) ->
          void $ sendTo sock (LBS.toStrict $ encode (Welcome pid gsSnap)) a
      when (mode == Solo) $ do
        (shouldSend, pid, gsSnap) <- atomically $ do
          cs <- readTVar (clients state)
          gs <- readTVar (gameState state)
          let humans = filter ((== Human) . playerType) (gamePlayer gs)
              chosen = case humans of
                         []     -> Player1
                         (h:_)  -> playerID h
              already = Map.lookup chosen cs == Just clientAddr
              csNoAddr = Map.filter (/= clientAddr) cs
              cs' = Map.insert chosen clientAddr csNoAddr
          writeTVar (clients state) cs'
          return (not already, chosen, gs)
        when shouldSend $ do
          void $ sendTo sock (LBS.toStrict $ encode (Welcome pid gsSnap)) clientAddr
    PauseRequest            -> atomically $ do
      modifyTVar' (gameState state) $ \gs ->
        if gameMode gs `elem` [Solo, CoopBot]
          then gs { gamePaused = True }
          else gs
    ResumeRequest           -> atomically $ do
      modifyTVar' (gameState state) $ \gs ->
        if gameMode gs `elem` [Solo, CoopBot]
          then gs { gamePaused = False }
          else gs
    _                       -> return ()

-- Helper to reset round when mode changes while preserving connected human players
handleSetMode :: ServerState -> GameMode -> STM ()
handleSetMode state mode = modifyTVar' (gameState state) $ \gs ->
  let
      resetPos pid =
        case mode of
          PvP ->
            case pid of
              Player1 -> Position (-100) (-250)
              Player2 -> Position 100 250
          _ ->
            case pid of
              Player1 -> Position (-100) (-250)
              Player2 -> Position 100 (-250)
      resetHuman p =
        if playerType p == Human
          then p { playerPos = resetPos (playerID p)
                 , playerLives = 3
                 , playerScore = 0
                 , playerDeaths = 0
                 , playerAction = Idle
                 , playerWantsToShoot = False
                 , playerShootCooldown = 0
                 , playerRespawnTimer = 0
                 , playerDamageTaken = 0
                 , playerBotState = BotCombat }
          else p
      humans = filter ((== Human) . playerType) (gamePlayer gs)
      resetPlayers = map resetHuman humans
  in gs { gameMode = mode
        , gamePlayer = case mode of
            Solo ->
              let pickOne = case resetPlayers of
                    [] -> []
                    hs -> let mP1 = filter ((== Player1) . playerID) hs
                          in if not (null mP1) then [head mP1] else [head hs]
              in pickOne
            _ -> resetPlayers
        , gameEnemies = []
        , gameItems = []
        , gameBullets = []
        , gameEnemiesLeft = 0
        , gameEnemiesSpawned = 0
        , gameSpawnTimer = 5.0
        , gameEnemySpawnTimer = 2.0
        , gameLevel = 0
        , gameEnemiesEscaped = 0
        , isShooting = False
        , gamePaused = False }

-- Variant that keeps the player who sent the request when switching to Solo
handleSetModeFromAddr :: ServerState -> GameMode -> SockAddr -> STM ()
handleSetModeFromAddr state mode addr = do
  cs <- readTVar (clients state)
  modifyTVar' (gameState state) $ \gs ->
    let
      resetPos pid =
        case mode of
          PvP -> case pid of
                   Player1 -> Position (-100) (-250)
                   Player2 -> Position 100 250
          _   -> case pid of
                   Player1 -> Position (-100) (-250)
                   Player2 -> Position 100 (-250)
      resetHuman p =
        if playerType p == Human
          then p { playerPos = resetPos (playerID p)
                 , playerLives = 3
                 , playerScore = 0
                 , playerDeaths = 0
                 , playerAction = Idle
                 , playerWantsToShoot = False
                 , playerShootCooldown = 0
                 , playerRespawnTimer = 0
                 , playerDamageTaken = 0
                 , playerBotState = BotCombat }
          else p
      humans = filter ((== Human) . playerType) (gamePlayer gs)
      resetPlayers = map resetHuman humans
      keepIdList = [ pid | (pid, a) <- Map.toList cs, a == addr ]
      selectedSolo =
        case keepIdList of
          (pid:_) ->
            let kept = filter ((== pid) . playerID) resetPlayers
            in if null kept
                 then let mP1 = filter ((== Player1) . playerID) resetPlayers
                      in if not (null mP1) then [head mP1] else take 1 resetPlayers
                 else kept
          [] -> let mP1 = filter ((== Player1) . playerID) resetPlayers
                in if not (null mP1) then [head mP1] else take 1 resetPlayers

      finalPlayers = case mode of
                       Solo -> selectedSolo
                       _    -> resetPlayers
    in gs { gameMode = mode
          , gamePlayer = finalPlayers
          , gameEnemies = []
          , gameItems = []
          , gameBullets = []
          , gameEnemiesLeft = 0
          , gameEnemiesSpawned = 0
          , gameSpawnTimer = 5.0
          , gameEnemySpawnTimer = 2.0
          , gameLevel = 0
          , gameEnemiesEscaped = 0
          , isShooting = False
          , gamePaused = False }

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
        
        isGameOver = case gameMode gs of
          PvP -> anyHumanDead
          _   -> allHumansDead || tooManyEscaped
    
    if isGameOver
      then return gs
      else do
        let humanCount = length $ filter ((== Human) . playerType) (gamePlayer gs)
            canPlay = case gameMode gs of
                        Solo    -> humanCount >= 1
                        CoopBot -> humanCount >= 1
                        Coop    -> humanCount >= 1
                        PvP     -> humanCount >= 1
        
        if gamePaused gs
          then return gs
          else if not canPlay
          then return gs
          else do
            -- GỌI HÀM PURE LOGIC
            let (tickedGs, bullets) = stepGame deltaT gs
            let finalGs = tickedGs { gameBullets = gameBullets tickedGs ++ bullets }
            writeTVar (gameState state) finalGs
            return finalGs

  let humans = filter ((== Human) . playerType) (gamePlayer updatedGameState)
      allHumansDead = not (null humans) && all ((<= 0) . playerLives) humans
      anyHumanDead = not (null humans) && any ((<= 0) . playerLives) humans
      tooManyEscaped = gameEnemiesEscaped updatedGameState >= 3
      
      isGameOver = case gameMode updatedGameState of
        PvP -> anyHumanDead
        _   -> allHumansDead || tooManyEscaped
      
      eliminatedPlayers = [ playerID p | p <- humans, playerLives p <= 0 ]
      baseMsg 
        | isGameOver = GameOver False
        | otherwise = UpdateGame updatedGameState
      baseEncoded = LBS.toStrict $ encode baseMsg
  
  -- Broadcast base update / game over
  forM_ (Map.elems clientMap) $ \addr -> sendTo sock baseEncoded addr
  
  when (not (null eliminatedPlayers) && not isGameOver && gameMode updatedGameState `elem` [Coop, PvP]) $ do
    forM_ eliminatedPlayers $ \pid -> do
      let elimMsg = PlayerEliminated pid
      let emBytes = LBS.toStrict $ encode elimMsg
      forM_ (Map.elems clientMap) $ \addr -> sendTo sock emBytes addr
  
  threadDelay (floor (deltaT * 1000 * 1000))
  

-- Join/leave/actions
handleJoinGame :: Socket -> ServerState -> PlayerID -> SockAddr -> IO ()
handleJoinGame sock state requestedPID addr = do
  putStrLn $ "Player " ++ show requestedPID ++ " wants to join from " ++ show addr
  (actualPID, welcomeState, newCount, full) <- atomically $ do
    clientsMap <- readTVar (clients state)
    gs <- readTVar (gameState state)
    let mode     = gameMode gs
        existing = map playerID (gamePlayer gs)
        humans   = filter ((== Human) . playerType) (gamePlayer gs)
        -- Offline modes always use Player1
        targetPID = case mode of
                      Solo    -> Player1
                      CoopBot -> Player1
                      Coop    -> if requestedPID == Player1 && Player1 `elem` existing
                                   then Player2 else requestedPID
                      PvP     -> if requestedPID == Player1 && Player1 `elem` existing
                                   then Player2 else requestedPID
        fullGame = mode `elem` [Coop, PvP] && Player1 `elem` existing && Player2 `elem` existing
    if fullGame
      then return (requestedPID, gs, Map.size clientsMap, True)
      else do
        let startPos = case mode of
              PvP -> case targetPID of
                       Player1 -> Position (-100) (-250)
                       Player2 -> Position 100 250
              _   -> if targetPID == Player1 then Position (-100) (-250) else Position 100 (-250)
            newPlayer = Player targetPID startPos 3 0 0 Idle False Human 0.0 0.0 0 BotCombat
            clientsMap' = case mode of
                            Solo    -> Map.singleton Player1 addr
                            CoopBot -> Map.singleton Player1 addr
                            _       -> Map.insert targetPID addr clientsMap
        writeTVar (clients state) clientsMap'
        let filteredOthers = filter ((/= targetPID) . playerID) (gamePlayer gs)
            finalPlayers  = case mode of
                              Solo    -> [newPlayer]
                              CoopBot -> [newPlayer]  -- bot added by logic
                              _       -> newPlayer : filteredOthers
            gs' = gs { gamePlayer = finalPlayers }
        writeTVar (gameState state) gs'
        return (targetPID, gs', Map.size clientsMap', False)
  if full
    then putStrLn "  Game is full! Cannot join."
    else do
      putStrLn $ "  Assigning player slot: " ++ show actualPID
      putStrLn $ "Player " ++ show actualPID ++ " joined successfully"
      putStrLn $ "Total clients: " ++ show newCount
      void $ sendTo sock (LBS.toStrict $ encode (Welcome actualPID welcomeState)) addr

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