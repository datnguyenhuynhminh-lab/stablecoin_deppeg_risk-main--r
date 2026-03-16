# ==============================================================================
# 1. KHAI BÁO THƯ VIỆN & CẤU HÌNH ĐƯỜNG DẪN
# ==============================================================================
library(httr)      # Thay cho requests
library(tidyverse) # Thay cho pandas
library(jsonlite)  # Để xử lý dữ liệu JSON
library(lubridate) # Xử lý thời gian

# Cấu hình mốc thời gian
START_DATE <- as.Date("2019-01-01")
END_DATE   <- as.Date("2026-01-01")

# ĐƯỜNG DẪN ĐẦU RA (THEO CẤU TRÚC VS CODE)
RAW_DATA_PATH <- "data/raw"
OUTPUT_FILENAME <- file.path(RAW_DATA_PATH, "defillama_total_stablecoin_cap.csv")

API_URL <- "https://stablecoins.llama.fi/stablecoincharts/all"

# ==============================================================================
# 2. HÀM TỰ ĐỘNG CHUẨN BỊ MÔI TRƯỜNG (SETUP FOLDERS)
# ==============================================================================
setup_environment <- function() {
  # Tự động tạo thư mục data/raw nếu chưa tồn tại
  if (!dir.exists(RAW_DATA_PATH)) {
    message(paste("[INFO] Đang tạo thư mục lưu trữ dữ liệu thô:", RAW_DATA_PATH))
    dir.create(RAW_DATA_PATH, recursive = TRUE)
  }
}

# ==============================================================================
# 3. HÀM CRAWL & XỬ LÝ DỮ LIỆU
# ==============================================================================
get_total_stablecoin_cap <- function() {
  message(paste("[INFO] Đang gọi API DefiLlama:", API_URL, "..."))
  
  tryCatch({
    # Gọi API với Header giả lập
    response <- GET(API_URL, add_headers(`User-Agent` = "Mozilla/5.0"), timeout(20))
    
    if (status_code(response) == 200) {
      # Parse JSON và làm phẳng cấu trúc lồng nhau (flatten = TRUE)
      data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
      
      if (length(data) == 0) {
        message("[WARNING] API trả về dữ liệu rỗng.")
        return(NULL)
      }
      
      # Chuyển vào Data Frame (Tibble)
      df <- as_tibble(data)
      
      # Kiểm tra cột cần thiết
      cols_needed <- c("date", "totalCirculatingUSD.peggedUSD")
      if (!all(cols_needed %in% names(df))) {
        message(paste("[ERROR] Thiếu cột dữ liệu. Các cột hiện có:", paste(names(df), collapse = ", ")))
        return(NULL)
      }
      
      # Xử lý và làm sạch dữ liệu
      df_final <- df %>%
        # Unix timestamp -> Date
        mutate(Date = as.Date(as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "UTC"))) %>%
        # Đổi tên cột
        rename(Total_Stable_Cap = totalCirculatingUSD.peggedUSD) %>%
        # Lọc theo ngày
        filter(Date >= START_DATE & Date <= END_DATE) %>%
        # Chọn cột
        select(Date, Total_Stable_Cap) %>%
        # Sắp xếp
        arrange(Date)
      
      return(df_final)
      
    } else {
      message(paste("[ERROR] Lỗi HTTP", status_code(response)))
      return(NULL)
    }
    
  }, error = function(e) {
    message(paste("[ERROR] Ngoại lệ xảy ra:", e$message))
    return(NULL)
  })
}

# ==============================================================================
# 4. THỰC THI (MAIN)
# ==============================================================================

# Bước 1: Đảm bảo thư mục đích tồn tại
setup_environment()

# Bước 2: Thực hiện crawl dữ liệu
df <- get_total_stablecoin_cap()

if (!is.null(df)) {
  # Định dạng hiển thị số thực (không dùng e+11)
  options(scipen = 999) 
  
  cat("\n--- KẾT QUẢ MẪU (5 dòng đầu) ---\n")
  print(head(df))
  
  # Bước 3: Lưu file CSV vào data/raw/
  write.csv(df, OUTPUT_FILENAME, row.names = FALSE)
  
  message(paste("\n[SUCCESS] Đã lưu dữ liệu vào đúng folder:", OUTPUT_FILENAME))
  message(paste("Tổng số dòng:", nrow(df)))
} else {
  message("[FAIL] Không lấy được dữ liệu.")
}