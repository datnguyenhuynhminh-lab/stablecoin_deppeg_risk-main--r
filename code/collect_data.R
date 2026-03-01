# ==============================================================================
# 1. KHAI BÁO THƯ VIỆN & CẤU HÌNH ĐƯỜNG DẪN
# ==============================================================================
library(tidyverse) # Thay cho pandas, numpy
library(lubridate) # Xử lý ngày tháng
library(slider)    # Để tính rolling statistics (Volatility)

# Cấu hình đường dẫn theo cấu trúc VS Code của bạn
INPUT_DIR   <- "data/raw/price"
OUTPUT_DIR  <- "data/processed"
MASTER_FILE <- file.path(OUTPUT_DIR, "master_stablecoin_research_final.csv")

# Cấu hình mốc thời gian và logic phân loại
START_DATE <- as.Date("2024-01-01")
END_DATE   <- as.Date("2026-01-01")
EXPECTED_DAYS <- as.numeric(END_DATE - START_DATE) + 1

FIAT_KEYWORDS <- c('Tether', 'USDC', 'PayPal', 'First Digital')

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
# 3. XỬ LÝ DỮ LIỆU TỪNG FILE
# ==============================================================================
process_stablecoin_research <- function() {
  setup_environment()
  
  # Tìm danh sách file CSV trong data/raw/price
  raw_files <- list.files(path = INPUT_DIR, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(raw_files) == 0) {
    stop(paste("Lỗi: Không tìm thấy file CSV nào trong", INPUT_DIR))
  }
  
  message(paste("Bắt đầu xử lý", length(raw_files), "file. Mục tiêu:", EXPECTED_DAYS, "ngày/file."))
  
  all_data <- list()
  data_summary <- list()
  
  for (file_path in raw_files) {
    file_name <- basename(file_path)
    coin_id <- str_split(file_name, "_")[[1]][1]
    
    tryCatch({
      # 1. Đọc dữ liệu (Sử dụng dấu phân cách ';' theo bản Python)
      df <- read_delim(file_path, delim = ";", show_col_types = FALSE)
      
      cols_needed <- c("timeClose", "close", "marketCap", "volume", "circulatingSupply")
      if (!all(cols_needed %in% names(df))) {
        message(paste(" -> Bỏ qua", file_name, ": Thiếu cột dữ liệu."))
        next
      }
      
      # 2. Tiền xử lý và lọc ngày
      df_clean <- df %>%
        select(all_of(cols_needed)) %>%
        rename(Date = timeClose, Price = close, Market_Cap = marketCap, Volume = volume, Supply = circulatingSupply) %>%
        mutate(Date = as.Date(ymd_hms(Date))) %>%
        filter(Date >= START_DATE & Date <= END_DATE) %>%
        distinct(Date, .keep_all = TRUE)
      
      actual_days_raw <- nrow(df_clean)
      if (actual_days_raw == 0) {
        message(paste(" ->", coin_id, ": Không có dữ liệu trong khoảng thời gian yêu cầu."))
        next
      }
      
      # 3. Làm sạch và điền đầy dữ liệu (Reindex & ffill)
      full_range <- tibble(Date = seq(START_DATE, END_DATE, by = "day"))
      
      df_filled <- full_range %>%
        left_join(df_clean, by = "Date") %>%
        arrange(Date) %>%
        fill(Price, Market_Cap, Volume, Supply, .direction = "down") # Forward fill
      
      # 4. Phân loại và tính toán chỉ số
      df_filled <- df_filled %>%
        mutate(
          Coin = coin_id,
          Category = if_else(any(str_detect(file_name, FIAT_KEYWORDS)), "Fiat-backed", "Crypto-backed"),
          Log_Return = log(Price / lag(Price)),
          # Tính độ lệch chuẩn trượt 30 ngày (Volatility)
          Volatility_30d = slide_dbl(Log_Return, sd, .before = 29, .complete = FALSE),
          Peg_Deviation = abs(Price - 1.0)
        )
      
      # Lưu tóm tắt độ phủ dữ liệu
      coverage <- (actual_days_raw / EXPECTED_DAYS) * 100
      data_summary[[coin_id]] <- tibble(
        Coin = coin_id,
        Start = min(df_clean$Date),
        End = max(df_clean$Date),
        Raw_Days = actual_days_raw,
        `Coverage_%` = round(coverage, 2),
        Status = if_else(actual_days_raw >= EXPECTED_DAYS, "Đủ", "Thiếu")
      )
      
      all_data[[coin_id]] <- df_filled
      message(paste(" ->", coin_id, ": Đã xử lý xong."))
      
    }, error = function(e) {
      message(paste(" -> Lỗi tại file", file_name, ":", e$message))
    })
  }
  
  # --- IN BẢNG KIỂM TRA TÍNH ĐẦY ĐỦ ---
  cat("\n", rep("=", 80), "\n", sep = "")
  cat(format(paste("BẢNG KIỂM TRA ĐỘ PHỦ DỮ LIỆU (Mục tiêu:", EXPECTED_DAYS, "ngày)"), width = 80, justify = "centre"), "\n")
  cat(rep("=", 80), "\n", sep = "")
  if (length(data_summary) > 0) {
    print(as.data.frame(bind_rows(data_summary)), row.names = FALSE)
  }
  cat(rep("=", 80), "\n", sep = "")
  
  # --- XUẤT FILE TỔNG HỢP ---
  if (length(all_data) > 0) {
    master_df <- bind_rows(all_data)
    write_csv(master_df, MASTER_FILE)
    message(paste("\n=> Đã lưu file Master tại:", MASTER_FILE))
  }
}

# Thực thi
process_stablecoin_research()