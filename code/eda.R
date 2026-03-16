# ==============================================================================
# Script Name: eda_models.R
# Purpose: Khám phá dữ liệu (EDA), thống kê mô tả và trực quan hóa phục vụ
#          cho mô hình GLM Binomial dự báo rủi ro Depeg của Stablecoin.
# Input: data/processed/stablecoin_panel_full_features.csv
# Output: outputs/eda_results/ (chứa các file .csv và .png)
# ==============================================================================

# 1. Setup & Libraries ----
message("[INFO] Đang nạp các thư viện cần thiết...")
suppressPackageStartupMessages({
  library(tidyverse) # Thao tác dữ liệu (dplyr, tidyr) và vẽ biểu đồ (ggplot2)
  library(skimr)     # Thống kê mô tả chi tiết
  library(corrplot)  # Vẽ heatmap tương quan
})

# Cấu hình đường dẫn
input_file <- "data/processed/stablecoin_panel_full_features.csv"
output_dir <- "outputs/eda_results"

# Tự động tạo thư mục output nếu chưa tồn tại
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("[INFO] Đã tạo thư mục: ", output_dir)
}

# 2. Nạp dữ liệu và Tiền xử lý (Data Ingestion) ----
message("[INFO] Đang đọc dữ liệu từ: ", input_file)
df <- read_csv(input_file, show_col_types = FALSE)

# Định dạng lại các kiểu dữ liệu cho đúng cấu trúc Panel Data
df <- df %>%
  mutate(
    date = as.Date(date),
    coin = as.factor(coin),
    # dummy_crypto = 1 (Crypto-backed), 0 (Fiat-backed)
    dummy = factor(dummy_crypto, levels = c(0, 1), labels = c("Fiat-backed", "Crypto-backed")),
    # depeg_static là label chính (1 = depeg, 0 = normal)
    depeg_static = as.factor(depeg_01)
  )

message("[INFO] Kích thước dữ liệu: ", nrow(df), " dòng, ", ncol(df), " cột.")

# 3. Thống kê mô tả (Descriptive Statistics) ----
message("[INFO] Đang thực hiện thống kê mô tả...")

# Bảng 1: Tỷ lệ Depeg tổng thể và theo Coin / Loại tài sản đảm bảo
depeg_summary <- df %>%
  group_by(coin, dummy) %>%
  summarise(
    total_obs = n(),
    depeg_events = sum(as.numeric(as.character(depeg_static)), na.rm = TRUE),
    depeg_rate_pct = round((depeg_events / total_obs) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(depeg_rate_pct))

write_csv(depeg_summary, file.path(output_dir, "table1_depeg_rates.csv"))

# Bảng 2: Thống kê tóm tắt các biến độc lập (Summary Stats)
# Chỉ chọn các biến numeric quan trọng và các biến lag dùng cho mô hình
numeric_vars <- df %>%
  select(
    dev_abs, sigma_dev_30d, log_volume, illiq, # circulating_supply_percent_change_7d,
    BTC_Realized_Daily_Volatility, ETH_percent_change_24h, stable_mcap_logret, fear_greed_index,
    dev_abs_lag1, sigma_dev_30d_lag1, log_volume_lag1, illiq_lag1
  )

summary_stats <- skim(numeric_vars)
# Chuyển đổi kết quả skimr sang dạng bảng chuẩn để lưu CSV
write_csv(as_tibble(summary_stats), file.path(output_dir, "table2_summary_statistics.csv"))

# Bảng 3: Phân tích Missing Data
missing_data <- df %>%
  summarise(across(everything(), ~ round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Pct") %>%
  arrange(desc(Missing_Pct))

write_csv(missing_data, file.path(output_dir, "table3_missingness_table.csv"))
message("[INFO] Đã lưu các bảng thống kê vào thư mục output.")

# 4. Trực quan hóa dữ liệu (Visualizations) ----
message("[INFO] Đang vẽ biểu đồ...")

# Cài đặt theme mặc định cho ggplot
theme_set(theme_minimal() + theme(plot.title = element_text(face = "bold")))

# Biểu đồ 1: Tần suất Depeg theo Đồng coin và Loại tài sản bảo đảm
p1 <- ggplot(depeg_summary, aes(x = reorder(coin, depeg_events), y = depeg_events, fill = dummy)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Fiat-backed" = "#2c3e50", "Crypto-backed" = "#e74c3c")) +
  labs(
    title = "Tần suất sự kiện Depeg theo Stablecoin",
    subtitle = "Phân loại theo cấu trúc tài sản bảo đảm (Fiat vs Crypto)",
    x = "Stablecoin",
    y = "Số lượng sự kiện Depeg (Ngày)",
    fill = "Backing Type"
  )
ggsave(file.path(output_dir, "plot1_depeg_frequency.png"), plot = p1, width = 8, height = 5, bg = "white")


# Figure 2 — Distribution của dev_abs và ngưỡng depeg (Bản cập nhật) ----
# Tinh chỉnh: Thu hẹp xlim xuống 0.02 và dùng scales = "free_y" để ôm sát phân phối
p2 <- ggplot(df, aes(x = dev_abs, fill = dummy)) +
  geom_density(alpha = 0.6, color = "black") +
  geom_vline(xintercept = 0.01, color = "red", linetype = "dashed", size = 1) +
  # Dịch chuyển chữ sang trái một chút để không đè lên nét đứt
  annotate("text", x = 0.0105, y = 50, label = "Ngưỡng Depeg (1%)", color = "red", angle = 90) +
  # Thêm scales = "free_y" để chiều cao trục Y tự động ép sát theo từng panel
  facet_wrap(~ dummy, ncol = 1, scales = "free_y") +
  # Cắt đuôi tại 0.02 thay vì 0.05 để loại bỏ khoảng trắng thừa bên phải
  coord_cartesian(xlim = c(0, 0.02)) + 
  scale_fill_manual(values = c("Fiat-backed" = "#3498db", "Crypto-backed" = "#e67e22")) +
  labs(
    title = "Phân phối Độ lệch tuyệt đối (dev_abs)",
    subtitle = "So sánh giữa Fiat-backed và Crypto-backed. Trục X được thu hẹp (0-2%).",
    x = "Độ lệch tuyệt đối khỏi mốc 1 USD (|Price - 1|)",
    y = "Mật độ (Density)",
    fill = "Backing Type"
  ) + theme(legend.position = "none")

ggsave(file.path(output_dir, "Figure_2_Dev_Abs_Distribution.png"), plot = p2, width = 10, height = 6, dpi = 300)

# Biểu đồ 3: Biến động Macro (BTC Volatility) vs Tổng số Depeg trong ngày
# Tính tổng số depeg mỗi ngày
daily_depeg <- df %>%
  group_by(date) %>%
  summarise(
    total_depeg = sum(as.numeric(as.character(depeg_static)), na.rm = TRUE),
    btc_vol = mean(BTC_Realized_Daily_Volatility, na.rm = TRUE)
  ) %>% filter(!is.na(btc_vol))

# Tính hệ số scale cho trục Y phụ
coeff <- max(daily_depeg$btc_vol, na.rm = TRUE) / max(daily_depeg$total_depeg + 1, na.rm = TRUE)

p3 <- ggplot(daily_depeg, aes(x = date)) +
  geom_col(aes(y = total_depeg * coeff), fill = "#3498db", alpha = 0.6) +
  geom_line(aes(y = btc_vol), color = "#e67e22", size = 1) +
  scale_y_continuous(
    name = "BTC Realized Daily Volatility (Line)",
    sec.axis = sec_axis(~ . / coeff, name = "Số lượng Stablecoin Depeg (Bars)")
  ) +
  labs(
    title = "Sự lan truyền rủi ro: Biến động BTC và Sự kiện Depeg",
    x = "Thời gian"
  )
ggsave(file.path(output_dir, "plot3_macro_spillover_overlay.png"), plot = p3, width = 10, height = 5, bg = "white")

# Biểu đồ 4: Heatmap tương quan (Correlation Matrix) kiểm tra Đa cộng tuyến (VIF)
# Chỉ lấy các biến độc lập dạng số (numeric predictors) dùng trong Model 4 & 5
cor_vars <- df %>%
  select(
    dev_abs_lag1, sigma_dev_30d_lag1, log_volume_lag1, illiq_lag1, 
    BTC_Realized_Daily_Volatility_lag1, ETH_percent_change_24h_lag1, 
    stable_mcap_logret_lag1, fear_greed_index_lag1
  ) %>%
  drop_na() # Bỏ NA để tính correlation

cor_matrix <- cor(cor_vars)

png(file.path(output_dir, "plot4_correlation_heatmap.png"), width = 800, height = 800, res = 120)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.7,
         title = "Ma trận tương quan giữa các biến trễ (Lag1 Predictors)",
         mar=c(0,0,2,0))
invisible(dev.off())

# Figure 4 — Chuỗi giá (Price Series) và Ngưỡng Depeg của 8 Stablecoin ----
message("[INFO] Đang vẽ Figure 4 (Price Series Grid)...")
# Biểu đồ Chuỗi giá (Price Series) và Ngưỡng Depeg ----
message("[INFO] Đang vẽ Biểu đồ Price Series Grid...")
# Biểu đồ Chuỗi giá (Price Series) và Ngưỡng Depeg ----
message("[INFO] Đang vẽ Biểu đồ Price Series Grid...")

# 1. Khai báo danh sách thứ tự xếp chéo để tạo bố cục 2 cột trái (Crypto), 2 cột phải (Fiat)
target_order_grid <- c(
  # Hàng 1: 2 Crypto đầu, 2 Fiat đầu
  "GHO", "Legacy Frax Dollar", "BUSD", "First Digital USD",
  # Hàng 2: 2 Crypto tiếp theo, 2 Fiat tiếp theo
  "Liquity USD", "TerraClassicUSD", "PayPal USD", "Tether USDt",
  # Hàng 3: 2 Crypto cuối, 2 Fiat cuối
  "crvUSD", "sUSD", "TrueUSD", "USDC"
)

# 2. Xử lý dữ liệu ép thứ tự cho biểu đồ giá
df_fig4 <- df %>%
  filter(coin %in% target_order_grid) %>%
  mutate(coin = factor(coin, levels = target_order_grid))

# Lọc ra các điểm depeg để chấm đỏ
depeg_points <- df_fig4 %>% filter(depeg_static == 1)

# 3. Vẽ biểu đồ lưới 3x4
p4 <- ggplot(df_fig4, aes(x = date, y = close)) +
  geom_line(color = "#2c3e50", size = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 1.00, color = "black", linetype = "solid", size = 0.4) +
  geom_hline(yintercept = 1.01, color = "#c0392b", linetype = "dashed", size = 0.6) +
  geom_hline(yintercept = 0.99, color = "#c0392b", linetype = "dashed", size = 0.6) +
  geom_point(data = depeg_points, aes(x = date, y = close), color = "red", size = 1.5) +
  # Chia lưới 4 cột, do đã ép levels ở trên nên nó sẽ tự động đổ đúng vị trí 2 trái - 2 phải
  facet_wrap(~ coin, ncol = 4, scales = "free_y") +
  labs(
    title = "Biến động giá Stablecoin và Các ngưỡng Depeg (\u00B11%)",
    subtitle = "2 cột trái: Crypto/Algo-backed. 2 cột phải: Fiat-backed. Chấm đỏ: Sự kiện mất chốt.",
    x = "Thời gian",
    y = "Giá đóng cửa (USD)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10, color = "white"),
    strip.background = element_rect(fill = "#34495e"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(output_dir, "Figure_4_Price_Series_Grid.png"), plot = p4, width = 16, height = 10, dpi = 300)
# Biểu đồ 5: Dấu vết (Trailing) độ lệch chuẩn 30 ngày (sigma_dev_30d)
p5 <- df %>%
  filter(!is.na(sigma_dev_30d)) %>%
  ggplot(aes(x = date, y = sigma_dev_30d, color = coin)) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Biến động lịch sử 30 ngày (Rolling Volatility) của các Stablecoin",
    x = "Thời gian",
    y = "Sigma 30 Days",
    color = "Stablecoin"
  ) +
  theme(legend.position = "bottom")
ggsave(file.path(output_dir, "plot5_rolling_volatility.png"), plot = p5, width = 10, height = 5, bg = "white")

# Biểu đồ Vốn hóa thị trường (Market Capitalization) ----
message("[INFO] Đang vẽ Biểu đồ Market Cap...")

# 1. Khai báo danh sách thứ tự để phần Legend (chú thích) hiện chuẩn
target_order <- c(
  "GHO", "Legacy Frax Dollar", "BUSD", "First Digital USD",
  "Liquity USD", "TerraClassicUSD", "PayPal USD", "Tether USDt",
  "crvUSD", "sUSD", "TrueUSD", "USDC"
)

# 2. Xử lý dữ liệu
df_fig5 <- df %>%
  filter(coin %in% target_order) %>%
  mutate(coin = factor(coin, levels = target_order))

# Tạo dải 12 màu có độ tương phản cao, rực rỡ và dễ phân biệt nhất
high_contrast_colors <- c(
  "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", 
  "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff"
)

# 3. Vẽ biểu đồ
p5 <- ggplot(df_fig5, aes(x = date, y = market_cap / 1e9, color = coin)) +
  geom_line(size = 1, alpha = 0.9) +
  # Format lại trục năm: Khoảng cách 1 năm, chỉ hiện số năm (ví dụ: 2021, 2022)
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # Sử dụng dải màu mới
  scale_color_manual(values = high_contrast_colors) +
  labs(
    title = "Biến động Vốn hóa thị trường (Market Capitalization)",
    subtitle = "Giai đoạn 2020 - 2025",
    x = "Năm",
    y = "Vốn hóa thị trường (Tỷ USD)",
    color = "Stablecoin"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    # In đậm và tô màu Title giống tông màu xanh đen (#34495e) của Figure 4
    plot.title = element_text(face = "bold", size = 14, color = "#34495e"),
    # Để chữ ở trục X nằm ngang bình thường cho dễ đọc
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave(file.path(output_dir, "Figure_5_Market_Cap.png"), plot = p5, width = 10, height = 6, dpi = 300)
message("[INFO] Hoàn tất! Tất cả bảng biểu và hình ảnh đã được lưu vào: ", output_dir)
# ==============================================================================