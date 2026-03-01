🪙 Stablecoin Depeg Risk Analysis (R-based Pipeline)
1. Tổng quan dự án (Project Overview)
Dự án này được thiết kế như một pipeline tự động hóa hoàn toàn, từ khâu thu thập dữ liệu (Crawl) đến thực thi các mô hình kinh tế lượng để phân tích và dự báo rủi ro de-peg (mất neo giá trị 1 USD) của các Stablecoin phổ biến trong giai đoạn 2024–2026.

Câu hỏi nghiên cứu trọng tâm:
Inference: Yếu tố nào (thanh khoản, biến động thị trường, tâm lý) thực sự thúc đẩy rủi ro mất neo?

Mechanism: Có sự khác biệt có ý nghĩa thống kê về rủi ro giữa nhóm Fiat-backed (đảm bảo bằng tiền pháp định) và Crypto-backed (đảm bảo bằng thuật toán/tài sản số) không?

Prediction: Liệu các mô hình Logistic với sai số chuẩn cụm (Cluster SE) có đủ khả năng cảnh báo sớm các biến cố hiếm này?

2. Cấu trúc thư mục (Project Structure)
Dự án tuân thủ tiêu chuẩn tổ chức dữ liệu khoa học, tách biệt rõ ràng giữa mã nguồn, dữ liệu thô và kết quả đầu ra:

Plaintext
.
├── code/                    # Mã nguồn xử lý (R scripts)
│   ├── crawl_top5.R         # Tải dữ liệu BTC/ETH từ Yahoo Finance
│   ├── crawl_market_cap.R   # Tải vốn hóa stablecoin từ DefiLlama
│   ├── crawl_fear_and_greed.R # Tải chỉ số tâm lý thị trường
│   ├── collect_data.R       # Gộp và làm sạch dữ liệu thô (Raw -> Master)
│   ├── panel_2.R            # Feature Engineering & tạo biến trễ (Lag)
│   └── simple_model.R       # Hồi quy Logit & Xuất kết quả Excel
├── data/                    # Quản lý dữ liệu
│   ├── raw/                 # Dữ liệu thô CSV trực tiếp từ API
│   │   └── price/           # Lịch sử giá chi tiết từng đồng coin
│   ├── processed/           # Dữ liệu đã qua xử lý (Master & Panel)
│   └── output/              # Biểu đồ và hình ảnh trực quan (PNG)
├── model/                   # Kết quả mô hình (Excel & Diagnostics)
├── renv/                    # Môi trường ảo của R (Quản lý thư viện)
├── renv.lock                # Danh sách chi tiết các package và phiên bản
└── README.md                # Tài liệu hướng dẫn này
3. Quy trình vận hành (Data Pipeline)
Dự án vận hành theo một vòng đời khép kín để đảm bảo tính nhất quán của kết quả nghiên cứu:

Thu thập (Crawl): Tự động hóa việc lấy dữ liệu từ DefiLlama, Alternative.me và Yahoo Finance.

Hợp nhất (Merge): collect_data.R thực hiện gộp dữ liệu 8 đồng coin, xử lý dữ liệu thiếu bằng phương pháp Forward Fill để đảm bảo tính liên tục của chuỗi thời gian.

Biến đổi (Feature Engineering): panel_2.R tính toán Volatility 30d, Illiquidity và đặc biệt là tạo Biến trễ (Lag 1) cho mọi biến độc lập nhằm khử hiện tượng nội sinh (Endogeneity).

Phân tích (Modeling): simple_model.R thực thi hồi quy Logistic sử dụng Cluster Standard Errors theo từng đồng coin để xử lý vấn đề tương quan trong nhóm.

4. Danh mục biến số (Feature Set)
Mô hình sử dụng đa dạng các nhóm biến độc lập để tăng tính giải thích:

Stablecoin-level: dev_abs (độ lệch giá), sigma_dev_30d (biến động), illiq (thanh khoản kém).

Market Spillover: Biến động giá và lợi suất của hai tài sản dẫn dắt thị trường là BTC và ETH.

Macro & Sentiment: Vốn hóa thị trường Stablecoin tổng thể và Chỉ số Fear & Greed.

Interaction terms: Hiệu ứng tương tác giữa loại tài sản bảo đảm (Dummy) và các biến đặc thù của coin.

5. Hướng dẫn sử dụng & Cài đặt
Dự án tích hợp renv để bất kỳ ai cũng có thể tái lập môi trường nghiên cứu một cách chính xác.

Thiết lập môi trường:
Mở dự án bằng RStudio hoặc VS Code.

Nhập lệnh sau vào Console để tự động tải các Package cần thiết: renv::restore().

Chạy mô hình:
Để thực hiện toàn bộ quy trình và nhận file kết quả cuối cùng, hãy chạy lệnh:

R
source("code/simple_model.R")
Kết quả sẽ xuất hiện tại thư mục model/ dưới định dạng Excel, bao gồm 3 trang: Coefficients (Hệ số), Summary (AUC, F1-score) và VIF (Kiểm tra đa cộng tuyến).

6. Các kỹ thuật thống kê áp dụng (Key Technical Notes)
Ngưỡng De-peg: Sử dụng ngưỡng tĩnh ±1% để đánh giá biến cố.

Optimal Cutoff: Mô hình không dùng ngưỡng 0.5 mặc định mà tự động tìm ngưỡng tối ưu trên tập Train để đạt F1-score cao nhất.

Centering: Mọi biến liên tục đều được chuẩn hóa quanh giá trị trung bình tập Train để giảm thiểu đa cộng tuyến khi đưa vào các biến tương tác.