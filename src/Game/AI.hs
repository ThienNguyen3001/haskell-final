module Game.AI where

import Game.Data
import Game.Constants

-- Bot FSM logic: states and transitions
botStepFSM :: Float -> Player -> [Enemy] -> [Item] -> Player
botStepFSM deltaT p enemies items
  | playerLives p <= 0 = p { playerAction = Idle
                           , playerWantsToShoot = False
                           , playerBotState = BotDead }
  | otherwise =
      let cd = max 0 (playerShootCooldown p - deltaT)
          p0 = p { playerShootCooldown = cd, playerWantsToShoot = False }
          lowHP = playerLives p0 < 2
          mEnemy = closestEnemy p0 enemies
          mItem  = closestItem p0 items
          nextState = case playerBotState p0 of
                        BotDead   -> BotSpawn
                        BotSpawn  -> if lowHP then maybe BotLowHealth (const BotSeekItem) mItem else BotCombat
                        BotCombat -> if lowHP then maybe BotLowHealth (const BotSeekItem) mItem else BotCombat
                        BotLowHealth -> maybe BotLowHealth (const BotSeekItem) mItem
                        BotSeekItem  -> if not lowHP || null items then BotCombat else BotSeekItem
      in case nextState of
           BotSeekItem ->
             let p1 = maybe (idleBot p0 cd) (\it -> moveTowardsItem p0 it cd) mItem
                 p2 = maybe p1 (\e -> tryShootAt p1 e) mEnemy
             in p2 { playerBotState = BotSeekItem }
           BotLowHealth -> case mEnemy of
             Nothing -> (idleBot p0 cd) { playerBotState = BotLowHealth }
             Just e  -> (fightEnemiesCautious p0 e cd) { playerBotState = BotLowHealth }
           BotCombat -> case mEnemy of
             Nothing -> (idleBot p0 cd) { playerBotState = BotCombat }
             Just e  -> (fightEnemiesBalanced p0 e cd) { playerBotState = BotCombat }
           BotSpawn -> (idleBot p0 cd) { playerBotState = BotSpawn }
           BotDead  -> (idleBot p0 cd) { playerBotState = BotDead }

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

-- Helpers used by FSM
closestEnemy :: Player -> [Enemy] -> Maybe Enemy
closestEnemy p es = case es of
  [] -> Nothing
  _  -> let esd = map (\e -> (e, distance (playerPos p) (enemyPos e))) es
            (emin, _) = minimumBy (\(_,d1) (_,d2) -> compare d1 d2) esd
        in Just emin

closestItem :: Player -> [Item] -> Maybe Item
closestItem p is = case is of
  [] -> Nothing
  _  -> let isd = map (\i -> (i, distance (playerPos p) (itemPos i))) is
            (imin, _) = minimumBy (\(_,d1) (_,d2) -> compare d1 d2) isd
        in Just imin

tryShootAt :: Player -> Enemy -> Player
tryShootAt p e =
  let (Position px py) = playerPos p
      (Position ex ey) = enemyPos e
      dx = ex - px
      horizontallyAligned = abs dx < 50
      enemyAbove = ey > py - 20
      canShoot = playerShootCooldown p <= 0
  in p { playerWantsToShoot = horizontallyAligned && enemyAbove && canShoot }

fightEnemiesBalanced :: Player -> Enemy -> Float -> Player
fightEnemiesBalanced p e cd =
  let Position px py = playerPos p
      Position ex ey = enemyPos e
      dx = ex - px
      baseAction | abs dx < 12 = Idle
                 | dx < 0      = MoveLeft
                 | otherwise   = MoveRight
      -- If the bot is above the enemy, prefer moving down to engage vertically
      finalAction | ey + 5 < py = MoveDown
                  | otherwise   = baseAction
      p1 = p { playerAction = finalAction, playerShootCooldown = cd }
  in tryShootAt p1 e

fightEnemiesCautious :: Player -> Enemy -> Float -> Player
fightEnemiesCautious p e cd =
  let Position px py = playerPos p
      Position ex ey = enemyPos e
      dx = ex - px
      baseAction | abs dx < 15 = Idle
                 | dx < 0      = MoveLeft
                 | otherwise   = MoveRight
      finalAction | ey + 5 < py = MoveDown
                  | otherwise   = baseAction
      p1 = p { playerAction = finalAction, playerShootCooldown = cd }
  in tryShootAt p1 e

-- Helper function to calculate distance between two positions
distance :: Position -> Position -> Float
distance (Position x1 y1) (Position x2 y2) = 
    sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs
minimumBy _ [] = error "minimumBy: empty list"