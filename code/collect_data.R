# ==============================================================================
# 1. KHAI BÁO THƯ VIỆN & CẤU HÌNH ĐƯỜNG DẪN
# ==============================================================================
library(tidyverse) # Xử lý dữ liệu
library(lubridate) # Xử lý ngày tháng
library(slider)    # Để tính rolling statistics (Volatility)

# Cấu hình đường dẫn đầu vào (File tổng hợp chứa 15 coin)
INPUT_FILE <- "D:\\SCHOOL\\HK5 (25-26)\\GOI_2\\stablecoin_deppeg_risk-main--r\\data\\processed\\stablecoin_panel_data.csv"

# Cấu hình đường dẫn đầu ra
OUTPUT_DIR  <- "D:\\SCHOOL\\HK5 (25-26)\\GOI_2\\stablecoin_deppeg_risk-main--r\\data\\processed"
MASTER_FILE <- file.path(OUTPUT_DIR, "master_stablecoin_research_final.csv")

# Cấu hình mốc thời gian (Điều chỉnh rộng hơn để bao phủ dữ liệu dài hạn)
START_DATE <- as.Date("2020-01-01") # Nên để dài để giữ lại các sự kiện de-peg cũ
END_DATE   <- as.Date("2025-12-31")         # Lấy đến ngày hiện tại
EXPECTED_DAYS <- as.numeric(END_DATE - START_DATE) + 1

# Khai báo chính xác nhóm Fiat-backed dựa trên các coin đã kéo về
FIAT_COINS <- c('Tether USDt', 'USDC', 'First Digital USD', 'PayPal USD', 
                'TrueUSD', 'Pax Dollar', 'BUSD')

# ==============================================================================
# 2. HÀM TỰ ĐỘNG CHUẨN BỊ MÔI TRƯỜNG
# ==============================================================================
setup_environment <- function() {
  if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR, recursive = TRUE)
    message(paste("[INFO] Đã tạo thư mục đầu ra:", OUTPUT_DIR))
  }
}

# ==============================================================================
# 3. XỬ LÝ DỮ LIỆU TỪ FILE PANEL TỔNG
# ==============================================================================
process_stablecoin_research <- function() {
  setup_environment()
  
  if (!file.exists(INPUT_FILE)) {
    stop(paste("Lỗi: Không tìm thấy file CSV tại", INPUT_FILE))
  }
  
  message("Bắt đầu đọc file dữ liệu tổng hợp...")
  
  # 1. Đọc dữ liệu (File được tạo từ crypto2 dùng dấu phẩy mặc định)
  df_raw <- read_csv(INPUT_FILE, show_col_types = FALSE)
  
  # 2. Làm sạch cơ bản và đổi tên cột cho đồng nhất với code cũ
  df_clean <- df_raw %>%
    # Chuyển đổi timestamp thành Date
    mutate(Date = as.Date(timestamp)) %>%
    # Chỉ định các cột cần thiết
    select(
      Coin = name,
      Date = Date,
      Price = close,
      Market_Cap = marketCap,
      Volume = volume,
      Supply = circulatingSupply
    ) %>%
    # Lọc theo khung thời gian nghiên cứu
    filter(Date >= START_DATE & Date <= END_DATE) %>%
    # Xóa các dòng trùng lặp (nếu có)
    distinct(Coin, Date, .keep_all = TRUE)
  
  # 3. Tạo khung dữ liệu chuẩn (Panel chuẩn) để phát hiện và điền khuyết
  # Tìm danh sách tất cả các coin có trong data
  all_coins <- unique(df_clean$Coin)
  
  # Tạo một grid chứa tất cả các tổ hợp (Coin x Date)
  full_panel <- expand_grid(
    Coin = all_coins,
    Date = seq(START_DATE, END_DATE, by = "day")
  )
  
  message("Đang xử lý chuỗi thời gian, tính toán biến số và phân loại (Backing Type)...")
  
  # 4. Ghép dữ liệu, điền khuyết và tính toán
  df_processed <- full_panel %>%
    left_join(df_clean, by = c("Coin", "Date")) %>%
    group_by(Coin) %>%
    arrange(Date) %>%
    # Lọc bỏ khoảng thời gian trống trước khi coin ra mắt
    # Chỉ giữ lại từ ngày có dữ liệu giá đầu tiên của mỗi coin
    filter(Date >= min(Date[!is.na(Price)])) %>%
    # Điền khuyết (Forward fill) cho các ngày bị mất dữ liệu ở giữa
    fill(Price, Market_Cap, Volume, Supply, .direction = "down") %>%
    # Tính toán các biến động lực học
    mutate(
      Category = if_else(Coin %in% FIAT_COINS, "Fiat-backed", "Crypto-backed/Algorithmic"),
      Log_Return = log(Price / lag(Price)),
      # Tính Volatility trượt 30 ngày. Xử lý NA ở những dòng đầu
      Volatility_30d = slide_dbl(Log_Return, sd, .before = 29, .complete = FALSE),
      Peg_Deviation = abs(Price - 1.0)
    ) %>%
    ungroup()
  
  # 5. Tóm tắt độ phủ dữ liệu
  data_summary <- df_processed %>%
    group_by(Coin, Category) %>%
    summarise(
      Start_Date = min(Date),
      End_Date = max(Date),
      Total_Days = n(),
      Missing_Prices_After_Fill = sum(is.na(Price)),
      .groups = "drop"
    ) %>%
    arrange(Category, Coin)
  
  # --- IN BẢNG KIỂM TRA TÍNH ĐẦY ĐỦ ---
  cat("\n", rep("=", 90), "\n", sep = "")
  cat(format("BẢNG TÓM TẮT DỮ LIỆU STABLECOIN PANEL", width = 90, justify = "centre"), "\n")
  cat(rep("=", 90), "\n", sep = "")
  print(as.data.frame(data_summary), row.names = FALSE)
  cat(rep("=", 90), "\n", sep = "")
  
  # --- XUẤT FILE TỔNG HỢP LÀM SẠCH ---
  write_csv(df_processed, MASTER_FILE)
  message(paste("\n=> Đã xử lý xong! File Master (sẵn sàng cho mô hình Logistic) được lưu tại:", MASTER_FILE))
}

# Thực thi
process_stablecoin_research()