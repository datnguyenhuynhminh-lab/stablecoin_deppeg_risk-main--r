🪙 Stablecoin Depeg Risk Analysis (R-based Pipeline)

1. Tổng quan dự án (Project Overview)

Dự án này được thiết kế như một pipeline tự động hóa hoàn toàn, từ khâu thu thập dữ liệu (Crawl) đến thực thi các mô hình kinh tế lượng để phân tích và dự báo rủi ro de-peg (mất neo giá trị 1 USD) của các Stablecoin phổ biến trong giai đoạn 2024–2026.

Câu hỏi nghiên cứu trọng tâm:
Inference: Yếu tố nào (thanh khoản, biến động thị trường, tâm lý) thực sự thúc đẩy rủi ro mất neo?

Mechanism: Có sự khác biệt có ý nghĩa thống kê về rủi ro giữa nhóm Fiat-backed và Crypto-backed không?

Prediction: Liệu các mô hình Logistic với sai số chuẩn cụm (Cluster SE) có đủ khả năng cảnh báo sớm các biến cố hiếm này?

1. Cấu trúc thư mục (Project Structure)
Dự án tách biệt rõ ràng giữa mã nguồn, dữ liệu thô và kết quả đầu ra:

## 📂 Cấu trúc dự án (Project Structure)

Dự án được tổ chức theo mô chuẩn khoa học dữ liệu, tách biệt rõ ràng các giai đoạn từ thu thập đến phân tích:

stablecoin-depeg-analysis/

├── 📁 code/                   # Script xử lý chính (R Language)

│   ├── 🛠️ collect_data.R      # Gộp dữ liệu & Xử lý Missing Value (Forward Fill)

│   ├── 📈 panel_2.R           # Biến đổi dữ liệu (Feature Engineering & Lags)

│   ├── 🤖 simple_model.R      # Chạy mô hình Logit & Xuất kết quả cuối cùng

│       ├── crawl_top5.R       # Lấy dữ liệu BTC/ETH (Yahoo Finance)

│       ├── crawl_market_cap.R # Lấy vốn hóa Stablecoin (DefiLlama)

│       └── crawl_f&g.R        # Lấy chỉ số Fear & Greed (Alternative.me)

├── 📁 data/                   # Quản lý kho lưu trữ dữ liệu

│   ├── 📥 raw/                # Dữ liệu gốc chưa qua chỉnh sửa (CSV)

│   ├── 🔄 processed/          # Dữ liệu đã làm sạch (Master & Panel Data)

│   └── 📊 output/             # Kết quả trực quan hóa (Biểu đồ PNG)

│       ├── 📊 eda_results/        # Các bảng Table 1, 2, 3 và biểu đồ EDA

│       ├── 📈 simple_logit_results.xlsx # Kết quả chạy mô hình

│       └── 🖼️ figures            # Các biểu đồ Figure

├── 📁 renv/                   # Cấu hình môi trường ảo (R Environment)

├── 📄 renv.lock               # Tệp khóa phiên bản thư viện (Packages)

└── 📄 README.md               # Tài liệu hướng dẫn dự án

1. Quy trình vận hành (Data Pipeline)

Thu thập (Crawl): Tự động hóa lấy dữ liệu từ DefiLlama, Alternative.me và Yahoo Finance.

Hợp nhất (Merge): collect_data.R gộp dữ liệu 8 đồng coin, xử lý dữ liệu thiếu bằng phương pháp Forward Fill.

Biến đổi (Feature Engineering): panel_2.R tính toán Volatility 30d, Illiquidity và tạo Biến trễ (Lag 1) để khử hiện tượng nội sinh.

Phân tích (Modeling): simple_model.R thực thi thực thi hồi quy Logistic với Cluster Standard Errors theo từng nhóm đồng coin.

1. Danh mục biến số (Feature Set)

Stablecoin-level: dev_abs (độ lệch giá), sigma_dev_30d (biến động), illiq (thanh khoản kém).

Market Spillover: Biến động giá và lợi suất của BTC và ETH.

Macro & Sentiment: Vốn hóa thị trường Stablecoin tổng thể và Chỉ số Fear & Greed.

Interaction terms: Hiệu ứng tương tác giữa loại tài sản bảo đảm (Dummy) và các biến đặc thù của coin.

1. Hướng dẫn sử dụng & Cài đặt

Dự án sử dụng renv để đảm bảo tái lập môi trường chính xác.

Thiết lập môi trường:

Mở dự án bằng RStudio hoặc VS Code.

Nhập lệnh sau vào Console để khôi phục thư viện: renv::restore().

Chạy mô hình:

Thực thi toàn bộ quy trình bằng lệnh:

source("run_all.R")

Kết quả (Excel) sẽ xuất hiện tại thư mục data/outputs/

1. Kỹ thuật thống kê (Key Technical Notes)

Ngưỡng De-peg: Sử dụng ngưỡng tĩnh ±1%.

Optimal Cutoff: Tự động tìm ngưỡng tối ưu trên tập Train để đạt F1-score cao nhất.

Centering: Chuẩn hóa biến liên tục quanh giá trị trung bình để giảm đa cộng tuyến khi dùng biến tương tác.
