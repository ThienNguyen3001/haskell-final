# haskell-final

## Project Structure  
```
.
├── app
│   └── Client.hs
|   └── Server.hs
|   └── Main.hs
│
├── src                          
│   └── GameData.hs
|   └── GameMes.hs
|   └── GameRender.hs
└── .gitignore
└── haskell-game-server.cabal
└── README.md
```
---

Cụ thể:
* ```GameData.hs```: định nghĩa tất cả các kiểu dữ liệu cốt lõi của game, gồm có trạng thái game, các đối tượng trong game, các thông điệp giao tiếp giữa client server. (Đông) (12/10)
* ```Client```: kết nối, gửi lệnh và nhận cập nhật từ server. (Đông) (12/10)
* ```Server.hs```: khởi tạo server, lắng nghe kết nối, quản lý các client, chạy vòng lặp game trong một luồng riêng biệt, và phát đi các cập nhật trạng thái. (Thiện) (16/10)
