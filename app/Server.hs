module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM_, void)
import qualified Data.Map as Map
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Data.Maybe (mapMaybe, fromMaybe)

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

-- | Trạng thái game ban đầu
initialGameState :: GameState
initialGameState = GameState [] [] [] 0 0 0 0 0 [] False

-- | Hàm main của Server
main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting server on port 8080..."

    -- 1. Thiết lập địa chỉ
    addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram })) Nothing (Just "8080")
    let serverAddr = head addrInfos

    -- 2. Tạo Socket
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    bind sock (addrAddress serverAddr)

    -- 3. Khởi tạo Server State
    gsTVar <- newTVarIO initialGameState
    clientsTVar <- newTVarIO Map.empty
    let state = ServerState gsTVar clientsTVar

    putStrLn "Server started. Listening for clients..."

    -- 4. Chạy luồng nhận tin nhắn và luồng game loop
    _ <- forkIO $ receiverLoop sock state
    _ <- forkIO $ gameLoop sock state

    -- Giữ luồng chính tồn tại
    forever $ threadDelay (10 * 1000 * 1000)

-- | Luồng nhận tin nhắn từ các client
receiverLoop :: Socket -> ServerState -> IO ()
receiverLoop sock state = forever $ do
    (byteString, clientAddr) <- recvFrom sock 1024
    let clientMsg = decode (LBS.fromStrict byteString) :: ClientMessage
    
    -- Xử lý tin nhắn
    case clientMsg of
        JoinGame pID -> handleJoinGame sock state pID clientAddr
        PlayerAction pID action -> handlePlayerAction state pID action
        PlayerQuit pID -> handlePlayerQuit state pID
        _ -> return ()

-- | Luồng chạy logic game (khoảng 60 FPS)
gameLoop :: Socket -> ServerState -> IO ()
gameLoop sock state = forever $ do
    let deltaT = 0.016 -- 16ms
    
    -- 1. Chạy logic game và cập nhật state
    (updatedGameState, newBullets) <- atomically $ do
        gs <- readTVar (gameState state)
        let (tickedGs, bullets) = runGameLogic deltaT gs
        
        -- Thêm đạn mới vào state cho tick sau
        let finalGs = tickedGs { gameBullets = gameBullets tickedGs ++ bullets }
        writeTVar (gameState state) finalGs
        return (finalGs, bullets)

    -- 2. Gửi state mới cho tất cả client
    clientMap <- atomically $ readTVar (clients state)
    let updateMsg = UpdateGame updatedGameState
    let encodedMsg = LBS.toStrict $ encode updateMsg
    
    forM_ (Map.elems clientMap) $ \addr ->
        sendTo sock encodedMsg addr

    -- 3. Delay
    threadDelay (floor (deltaT * 1000 * 1000))

-- | Xử lý khi client tham gia (SỬA LOGIC NẶNG)
handleJoinGame :: Socket -> ServerState -> PlayerID -> SockAddr -> IO ()
handleJoinGame sock state pID addr = do
    putStrLn $ "Player " ++ show pID ++ " joined from " ++ show addr

    -- 1. Xác định vị trí xuất hiện dựa trên ID
    let startPos = if pID == Player1 
                   then Position (-100) (-250) -- Player1 bên trái
                   else Position 100 (-250)   -- Player2 bên phải

    let newPlayer = Player pID startPos 3 0 0 Idle False

    -- 2. Thêm vào state và map
    (welcomeState, newCount) <- atomically $ do
        -- Thêm client vào map (ghi đè nếu đã tồn tại)
        clientsMap <- readTVar (clients state)
        let newClientsMap = Map.insert pID addr clientsMap
        writeTVar (clients state) newClientsMap

        -- Cập nhật danh sách người chơi
        gs <- readTVar (gameState state)
        -- Lọc ra tất cả người chơi KHÁC pID hiện tại
        let otherPlayers = filter ((/= pID) . playerID) (gamePlayer gs)
        -- Thêm người chơi mới (hoặc đã cập nhật) vào danh sách
        let finalPlayers = newPlayer : otherPlayers
        
        let newGs = gs { gamePlayer = finalPlayers }
        writeTVar (gameState state) newGs
        
        return (newGs, Map.size newClientsMap)

    putStrLn $ "Total clients: " ++ show newCount
    -- 3. Gửi tin nhắn chào mừng (Welcome)
    let welcomeMsg = Welcome pID welcomeState
    void $ sendTo sock (LBS.toStrict $ encode welcomeMsg) addr

-- | Xử lý khi client thoát
handlePlayerQuit :: ServerState -> PlayerID -> IO ()
handlePlayerQuit state pID = do
    putStrLn $ "Player " ++ show pID ++ " quit."
    atomically $ do
        -- Xóa client
        clientsMap <- readTVar (clients state)
        writeTVar (clients state) (Map.delete pID clientsMap)
        
        -- Xóa player
        gs <- readTVar (gameState state)
        let updatedPlayers = filter ((/= pID) . playerID) (gamePlayer gs)
        writeTVar (gameState state) (gs { gamePlayer = updatedPlayers })

-- | Xử lý hành động của player
handlePlayerAction :: ServerState -> PlayerID -> Action -> IO ()
handlePlayerAction state pID action = atomically $ do
    gs <- readTVar (gameState state)
    
    let updateFunc p = if playerID p == pID
                       then case action of
                                -- Nếu là Shoot, bật cờ 'WantsToShoot'
                                Shoot -> p { playerWantsToShoot = True }
                                -- Nếu là hành động khác, cập nhật 'playerAction' (cho di chuyển)
                                _     -> p { playerAction = action }
                       else p
                       
    let newPlayers = map updateFunc (gamePlayer gs)
    writeTVar (gameState state) (gs { gamePlayer = newPlayers })

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
    let 
        -- 1. Xử lý di chuyển (dựa trên playerAction)
        (dx, dy) = case playerAction p of
            MoveUp    -> (0, playerSpeed * deltaT)
            MoveDown  -> (0, -playerSpeed * deltaT)
            MoveLeft  -> (-playerSpeed * deltaT, 0)
            MoveRight -> (playerSpeed * deltaT, 0)
            _         -> (0, 0) 

        (Position x y) = playerPos p
        newPos = Position (clamp (x + dx) (-screenWidth/2) (screenWidth/2))
                          (clamp (y + dy) (-screenHeight/2) (screenHeight/2))

        -- 2. Tạo đạn nếu cờ 'wantsToShoot' được bật
        newBullet = if playerWantsToShoot p
                    then [Bullet (Position x (y + entitySize)) PlayerOwned (Just $ playerID p)]
                    else []

        -- 3. Reset cờ 'wantsToShoot' (để không bắn liên tục)
        finalWantsToShoot = False

    in (p { playerPos = newPos, playerWantsToShoot = finalWantsToShoot }, newBullet)

-- | Cập nhật kẻ địch
updateEnemies :: Float -> [Enemy] -> ([Enemy], [Bullet])
updateEnemies deltaT enemies =
    let updatedEnemies = map (updateEnemy deltaT) enemies
        newBullets = [] -- Kẻ địch chưa biết bắn
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
    let 
        players = gamePlayer gs
        enemies = gameEnemies gs
        bullets = gameBullets gs

        playerBullets = filter ((== PlayerOwned) . bulletOwner) bullets
        enemyBullets  = filter ((== EnemyOwned) . bulletOwner) bullets 

        -- 1. Đạn người chơi vs Kẻ địch
        (hitEnemies, hitPlayerBullets, remainingPlayerBullets) = checkBulletEnemyCollisions playerBullets enemies

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

        -- Cập nhật điểm
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