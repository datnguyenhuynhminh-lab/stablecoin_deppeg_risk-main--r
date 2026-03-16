# Cài đặt các package cần thiết nếu chưa có
# install.packages("crypto2")
# install.packages("dplyr")
# install.packages("lubridate")

library(crypto2)
library(dplyr)
library(lubridate)

# 1. Danh sách 15 slug chính xác
target_slugs <- c(
  "tether", "usd-coin", "first-digital-usd", "paypal-usd", 
  "trueusd", "binance-usd", "gho", "liquity-usd", "crvusd", "frax", "terrausd", "susd"
)

# 2. Lấy danh sách coin và lọc triệt để chống lặp
print("Đang tải danh sách coin...")
all_coins <- crypto_list()

target_coins <- all_coins %>% 
  filter(slug %in% target_slugs) %>%
  # Chỉ cần nhóm theo slug và lấy dòng đầu tiên của mỗi slug
  group_by(slug) %>%
  slice(1) %>% 
  ungroup()

# Kiểm tra lại số lượng coin trước khi lấy lịch sử (Phải ra đúng 15)
print(paste("Số lượng coin sẽ lấy dữ liệu:", nrow(target_coins)))
print(target_coins[, c("name", "symbol", "slug")])

# 3. Lấy dữ liệu lịch sử (Ví dụ: Từ 01/01/2019 đến nay)
start_date <- "20190101"
end_date <- "20251231"

print("Đang crawl dữ liệu lịch sử (quá trình này có thể mất vài phút)...")
historical_data <- crypto_history(target_coins, start_date = start_date, end_date = end_date)

# 4. Làm sạch và format lại đúng các cột bạn yêu cầu
final_dataset <- historical_data %>%
  select(
    name = name,
    timestamp = time_open, # Giữ lại timestamp gốc
    timeOpen = time_open,
    timeClose = time_close,
    timeHigh = time_high,
    timeLow = time_low,
    open = open,
    high = high,
    low = low,
    close = close,
    volume = volume,
    marketCap = market_cap,
    circulatingSupply = circulating_supply
  ) %>%
  mutate(
    # Chuyển đổi định dạng thời gian cho dễ đọc trong mô hình Panel
    timestamp = as.Date(timestamp),
    timeOpen = as.POSIXct(timeOpen),
    timeClose = as.POSIXct(timeClose),
    timeHigh = as.POSIXct(timeHigh),
    timeLow = as.POSIXct(timeLow)
  ) %>%
  arrange(name, timestamp)
  # ==============================================================================
# 4.5 Thống kê mô tả số lượng quan sát của từng coin
# ==============================================================================
print("--- THỐNG KÊ MÔ TẢ DỮ LIỆU VỪA CRAWL ---")
summary_stats <- final_dataset %>%
  group_by(name) %>%
  summarise(
    Observations = n(),
    Start_Date = min(timestamp, na.rm = TRUE),
    End_Date = max(timestamp, na.rm = TRUE)
  ) %>%
  arrange(desc(Observations)) # Sắp xếp từ nhiều quan sát nhất xuống ít nhất

print(as.data.frame(summary_stats))
cat("\nTổng số quan sát của toàn bộ mẫu:", nrow(final_dataset), "\n\n")

# 5. Kiểm tra kết quả và lưu ra file CSV
head(final_dataset)
write.csv(final_dataset, "D:\\SCHOOL\\HK5 (25-26)\\GOI_2\\stablecoin_deppeg_risk-main--r\\data\\processed\\stablecoin_panel_data.csv", row.names = FALSE)
print("Đã lưu dữ liệu thành công vào file stablecoin_panel_data.csv!")