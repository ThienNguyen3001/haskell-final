{-# LANGUAGE DeriveGeneric #-}

module GameMes where

import GameData (Position, Action, GameState, PlayerID)
import GHC.Generics (Generic)
import Data.Binary (Binary)

------------------------------------------------------------
-- THÔNG ĐIỆP TỪ CLIENT GỬI LÊN SERVER
------------------------------------------------------------

-- | Dữ liệu mà client gửi lên server để thông báo hành động hoặc yêu cầu
data ClientMessage
  = JoinGame PlayerID                   -- yêu cầu tham gia game
  | PlayerMove PlayerID Position        -- gửi vị trí mới của người chơi
  | PlayerAction PlayerID Action        -- gửi hành động cụ thể (bắn, di chuyển, idle)
  | PlayerHit PlayerID                  -- báo bị trúng đạn
  | PlayerRespawn PlayerID              -- hồi sinh
  | PlayerQuit PlayerID                 -- rời khỏi game
  deriving (Show, Eq, Generic)

instance Binary ClientMessage 

------------------------------------------------------------
-- THÔNG ĐIỆP TỪ SERVER GỬI XUỐNG CLIENT
------------------------------------------------------------

-- | Dữ liệu mà server gửi tới client để cập nhật hoặc thông báo
data ServerMessage
  = Welcome PlayerID GameState          -- chào mừng khi tham gia, gửi trạng thái ban đầu
  | UpdateGame GameState                -- cập nhật toàn bộ trạng thái game
  | UpdatePartial PlayerID Position     -- chỉ cập nhật riêng vị trí player (nếu cần tiết kiệm dữ liệu -- tùy chọn)
  | PlayerJoined PlayerID               -- thông báo có người mới vào
  | PlayerLeft PlayerID                 -- thông báo người chơi rời khỏi game
  | GameOver Bool                       -- thông báo kết thúc trò chơi (True = thắng, False = thua)
  deriving (Show, Eq, Generic)

instance Binary ServerMessage 
------------------------------------------------------------
-- THÔNG ĐIỆP KẾT NỐI HỆ THỐNG (tuỳ chọn) -- Nếu không cần thiết có thể xóa
------------------------------------------------------------

-- | Thông điệp cấp hệ thống để điều khiển kết nối, đồng bộ 
data NetworkMessage
  = FromClient ClientMessage
  | FromServer ServerMessage
  deriving (Show, Eq, Generic)

instance Binary NetworkMessage
