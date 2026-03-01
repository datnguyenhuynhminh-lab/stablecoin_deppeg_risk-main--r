# ==============================================================================
# 1. KHAI BÁO THƯ VIỆN & CẤU HÌNH ĐƯỜNG DẪN
# ==============================================================================
library(tidyquant) # Thay cho yfinance
library(tidyverse) # Thay cho pandas

# Cấu hình mốc thời gian
START_DATE <- "2024-01-01"
END_DATE   <- "2026-02-01"

# Danh sách Tickers (Yahoo Finance format)
tickers <- c("BTC-USD", "ETH-USD")

# ĐƯỜNG DẪN ĐẦU RA (THEO CẤU TRÚC VS CODE)
RAW_DATA_PATH <- "data/raw"
OUTPUT_FILENAME <- file.path(RAW_DATA_PATH, "crypto_history_yfinance_clean.csv")

# ==============================================================================
# 2. HÀM TỰ ĐỘNG CHUẨN BỊ MÔI TRƯỜNG
# ==============================================================================
setup_environment <- function() {
  # Tạo thư mục data/raw nếu chưa có
  if (!dir.exists(RAW_DATA_PATH)) {
    dir.create(RAW_DATA_PATH, recursive = TRUE)
    message(paste("[INFO] Đã tạo thư mục:", RAW_DATA_PATH))
  }
}

# ==============================================================================
# 3. HÀM CRAWL & XỬ LÝ DỮ LIỆU
# ==============================================================================
fetch_crypto_prices <- function() {
  message(paste("[INFO] Đang tải dữ liệu từ Yahoo Finance cho:", paste(tickers, collapse = ", ")))
  
  # tq_get tự động tải và trả về một tibble sạch
  df_raw <- tq_get(tickers,
                   get  = "stock.prices",
                   from = START_DATE,
                   to   = END_DATE)
  
  if (nrow(df_raw) > 0) {
    # TIỀN XỬ LÝ (PREPROCESSING) tương đương bản Python
    df_clean <- df_raw %>%
      # 1. Chọn và đổi tên cột giống hệt bản Python
      select(date, symbol, price = close, volume) %>%
      # 2. Làm sạch cột symbol (BTC-USD -> BTC)
      mutate(symbol = str_remove(symbol, "-USD")) %>%
      # 3. Sắp xếp theo symbol và ngày
      arrange(symbol, date)
    
    return(df_clean)
  } else {
    return(NULL)
  }
}

# ==============================================================================
# 4. THỰC THI (MAIN)
# ==============================================================================

# Bước 1: Đảm bảo thư mục tồn tại
setup_environment()

# Bước 2: Lấy dữ liệu
df <- fetch_crypto_prices()

if (!is.null(df)) {
  # Hiển thị thống kê kiểm tra
  cat("\n--- KIỂU DỮ LIỆU ---\n")
  print(glimpse(df))
  
  cat("\n--- THỐNG KÊ THEO SYMBOL ---\n")
  print(df %>% group_by(symbol) %>% summarise(min_date = min(date), max_date = max(date), count = n()))
  
  # Bước 3: Lưu file vào đúng vị trí data/raw/
  write.csv(df, OUTPUT_FILENAME, row.names = FALSE)
  message(paste("\n[SUCCESS] Đã lưu dữ liệu vào:", OUTPUT_FILENAME))
  
} else {
  message("[FAIL] Không lấy được dữ liệu từ Yahoo Finance.")
}