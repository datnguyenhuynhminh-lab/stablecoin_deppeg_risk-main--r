# ==============================================================================
# 1. KHAI BÁO THƯ VIỆN & CẤU HÌNH ĐƯỜNG DẪN
# ==============================================================================
library(httr)      
library(tidyverse) 
library(jsonlite)  
library(lubridate) 

API_URL <- "https://api.alternative.me/fng/?limit=0"

# CẤU HÌNH ĐƯỜNG DẪN THEO CẤU TRÚC VS CODE
RAW_DATA_PATH <- "data/raw"
OUTPUT_PATH   <- "data/processed"

OUTPUT_CSV_NAME <- file.path(RAW_DATA_PATH, "crypto_fear_and_greed_index.csv")
OUTPUT_IMG_NAME <- file.path(OUTPUT_PATH, "fear_and_greed_chart.png")
FIG_SIZE <- c(14, 7) 

# ==============================================================================
# 2. HÀM TỰ ĐỘNG CHUẨN BỊ MÔI TRƯỜNG
# ==============================================================================
setup_environment <- function() {
  # Tạo thư mục data/raw nếu chưa có
  if (!dir.exists(RAW_DATA_PATH)) {
    dir.create(RAW_DATA_PATH, recursive = TRUE)
    message(paste("[INFO] Đã tạo thư mục:", RAW_DATA_PATH))
  }
  # Tạo thư mục data/output nếu chưa có
  if (!dir.exists(OUTPUT_PATH)) {
    dir.create(OUTPUT_PATH, recursive = TRUE)
    message(paste("[INFO] Đã tạo thư mục:", OUTPUT_PATH))
  }
}

# ==============================================================================
# 3. HÀM CRAWL DATA
# ==============================================================================
fetch_fear_and_greed_data <- function(url) {
  message(paste("[INFO] Đang gọi API:", url, "..."))
  
  tryCatch({
    response <- GET(url, timeout(10))
    stop_for_status(response)
    
    raw_content <- content(response, "text", encoding = "UTF-8")
    data_json <- fromJSON(raw_content)
    
    if (!"data" %in% names(data_json)) {
      stop("API không trả về trường 'data'.")
    }
    
    message(paste("[SUCCESS] Đã tải về", nrow(data_json$data), "bản ghi."))
    return(data_json$data)
    
  }, error = function(e) {
    message(paste("[ERROR] Lỗi:", e$message))
    return(NULL)
  })
}

# ==============================================================================
# 4. HÀM TIỀN XỬ LÝ (PREPROCESSING)
# ==============================================================================
process_data <- function(raw_data) {
  message("[INFO] Đang xử lý dữ liệu...")
  
  df <- as_tibble(raw_data) %>%
    mutate(
      timestamp = as_datetime(as.numeric(timestamp)),
      value = as.numeric(value)
    ) %>%
    arrange(timestamp)
  
  return(df)
}

# ==============================================================================
# 5. TRỰC QUAN HÓA (VISUALIZATION)
# ==============================================================================
visualize_data <- function(df, output_img) {
  message("[INFO] Đang vẽ biểu đồ...")
  
  tryCatch({
    p <- ggplot(df, aes(x = timestamp, y = value)) +
      annotate("rect", xmin = as_datetime(-Inf), xmax = as_datetime(Inf), ymin = 75, ymax = 100, fill = "green", alpha = 0.15) +
      annotate("rect", xmin = as_datetime(-Inf), xmax = as_datetime(Inf), ymin = 50, ymax = 75, fill = "lightgreen", alpha = 0.15) +
      annotate("rect", xmin = as_datetime(-Inf), xmax = as_datetime(Inf), ymin = 25, ymax = 50, fill = "orange", alpha = 0.15) +
      annotate("rect", xmin = as_datetime(-Inf), xmax = as_datetime(Inf), ymin = 0, ymax = 25, fill = "red", alpha = 0.15) +
      geom_line(color = "#333333", size = 0.5) +
      labs(
        title = "Crypto Fear & Greed Index History",
        x = "Date",
        y = "Index Value"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12)
      )
    
    ggsave(output_img, plot = p, width = FIG_SIZE[1], height = FIG_SIZE[2], dpi = 300)
    message(paste("[SUCCESS] Đã lưu biểu đồ tại:", output_img))
    
  }, error = function(e) {
    message(paste("[ERROR] Lỗi khi vẽ biểu đồ:", e$message))
  })
}

# ==============================================================================
# 6. THỰC THI (MAIN)
# ==============================================================================
# Bước 1: Chuẩn bị thư mục
setup_environment()

# Bước 2: Lấy và xử lý dữ liệu
raw_data <- fetch_fear_and_greed_data(API_URL)

if (!is.null(raw_data)) {
  df <- process_data(raw_data)
  
  cat("\n--- Dữ liệu mẫu (5 dòng đầu) ---\n")
  print(head(df))
  cat("------------------------------\n")
  
  # Bước 3: Lưu CSV vào data/raw
  write.csv(df, OUTPUT_CSV_NAME, row.names = FALSE)
  message(paste("[SUCCESS] Đã xuất file CSV:", OUTPUT_CSV_NAME))
  
  # Bước 4: Lưu hình ảnh vào data/output
  visualize_data(df, OUTPUT_IMG_NAME)
  
  message("\n[DONE] Hoàn tất quá trình.")
} else {
  message("[FAIL] Không thể lấy dữ liệu.")
}