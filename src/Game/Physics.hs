module Game.Physics where

import Game.Data
import Game.Constants

-- AABB collision using half extents on each axis (inclusive bounds to avoid tunneling at edges)
collidesAABB :: Position -> Position -> Float -> Float -> Bool
collidesAABB (Position x1 y1) (Position x2 y2) halfX halfY =
  abs (x1 - x2) <= halfX && abs (y1 - y2) <= halfY

-- Collision helpers
checkPvPCollisions :: [Bullet] -> [Player] -> ([Player], [Bullet], [Player])
checkPvPCollisions bullets players =
  let collisions =
        [ (b, p)
        | b <- bullets
        , p <- players
        , collidesAABB (bulletPos b) (playerPos p)
                       (bulletHalf + playerHalf)
                       (bulletHalf + playerHalf)
        , case bulletShooter b of
            Just shooterId -> shooterId /= playerID p  -- Don't hit yourself
            Nothing -> False
        ]
      hitPlayers = map snd collisions
      hitBullets = map fst collisions
      remainingBullets = filter (`notElem` hitBullets) bullets
  in (hitPlayers, remainingBullets, players)

checkBulletEnemyCollisions :: [Bullet] -> [Enemy] -> ([Enemy], [Bullet], [Bullet])
checkBulletEnemyCollisions bullets enemies =
  let collisions =
        [ (b, e)
        | b <- bullets
        , e <- enemies
        , collidesAABB (bulletPos b) (enemyPos e)
                       (bulletHalf + enemyHalf)
                       (bulletHalf + enemyHalf)
        ]
      hitEnemies = map snd collisions
      hitBullets = map fst collisions
      remainingBullets = filter (`notElem` hitBullets) bullets
  in (hitEnemies, hitBullets, remainingBullets)

checkBulletPlayerCollisions :: [Bullet] -> [Player] -> ([Player], [Bullet])
checkBulletPlayerCollisions bullets players =
  let collisions =
        [ (b, p)
        | b <- bullets
        , p <- players
        , collidesAABB (bulletPos b) (playerPos p)
                       (bulletHalf + playerHalf)
                       (bulletHalf + playerHalf)
        ]
      hitPlayers = map snd collisions
      hitBullets = map fst collisions
      remainingBullets = filter (`notElem` hitBullets) bullets
  in (hitPlayers, remainingBullets)