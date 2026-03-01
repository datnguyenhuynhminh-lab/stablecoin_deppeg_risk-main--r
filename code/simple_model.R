# ==============================================================================
# STABLECOIN DE-PEGGING LOGIT MODELS (R Version)
# Purpose : Chạy các mô hình hồi quy Logit để dự báo rủi ro De-peg
# Input   : stablecoin_panel_full_features.csv
# Output  : simple_logit_results.xlsx
# ==============================================================================

# 1. KHAI BÁO THƯ VIỆN ---------------------------------------------------------
library(tidyverse)  # Xử lý dữ liệu
library(fixest)     # Hồi quy Logit với Cluster SE (Cực nhanh & chuẩn)
library(pROC)       # Tính toán AUC
library(writexl)    # Xuất kết quả ra Excel
library(car)        # Kiểm tra đa cộng tuyến (VIF)
renv::snapshot()
# 2. CẤU HÌNH ĐƯỜNG DẪN --------------------------------------------------------
# Tương ứng với cấu trúc bạn đang để trong VS Code
PANEL_FILE <- "data/processed/stablecoin_panel_full_features.csv"
OUTPUT_DIR <- "model"
OUT_FILE   <- file.path(OUTPUT_DIR, "simple_logit_results.xlsx")

# Mốc thời gian Train/Test
TRAIN_START <- as.Date("2024-01-01")
TRAIN_END   <- as.Date("2024-12-31")
TEST_START  <- as.Date("2025-01-01")
TEST_END    <- as.Date("2025-12-31")
table(train_df$depeg_static, train_df$dummy)
Y_COL <- "depeg_static" # Biến phụ thuộc: De-peg (±1%)

# 3. CHUẨN BỊ MÔI TRƯỜNG & DỮ LIỆU ---------------------------------------------
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

panel <- read_csv(PANEL_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Tách tập Train và Test
train_df <- panel %>% filter(date >= TRAIN_START & date <= TRAIN_END)
test_df  <- panel %>% filter(date >= TEST_START & date <= TEST_END)

# Centering biến liên tục dựa trên trung bình tập Train (tránh Data Leakage)
dev_mean <- mean(train_df$dev_abs_lag1, na.rm = TRUE)
sig_mean <- mean(train_df$sigma_dev_30d_lag1, na.rm = TRUE)

apply_prep <- function(df) {
  df %>% mutate(
    dev_abs_c = dev_abs_lag1 - dev_mean,
    sigma_dev_30d_c = sigma_dev_30d_lag1 - sig_mean,
    dev_abs_c_x_dummy = dev_abs_c * dummy,
    sigma_dev_30d_c_x_dummy = sigma_dev_30d_c * dummy
  )
}

train_df <- apply_prep(train_df)
test_df  <- apply_prep(test_df)

# 4. ĐỊNH NGHĨA CÁC MÔ HÌNH ----------------------------------------------------
# Danh sách các tổ hợp biến độc lập
specs <- list(
  baseline    = c("dev_abs_lag1", "sigma_dev_30d_lag1", "dummy"),
  interaction = c("dev_abs_c", "sigma_dev_30d_c", "dummy", "dev_abs_c_x_dummy", "sigma_dev_30d_c_x_dummy"),
  extended    = c("dev_abs_c", "sigma_dev_30d_c", "dummy", "dev_abs_c_x_dummy", "sigma_dev_30d_c_x_dummy", 
                  "BTC_Realized_Daily_Volatility", "global_mcap_logret", "fear_greed_index"),
  dynamic     = c("illiq", "BTC_Realized_Daily_Volatility", "ETH_percent_change_24h", 
                  "global_mcap_logret", "fear_greed_index", "dummy")
)

# 5. HÀM THỰC THI HỒI QUY ------------------------------------------------------
run_model <- function(vars, name) {
  message(paste(">>> Đang chạy mô hình:", name))
  
  # Tạo công thức hồi quy
  fml <- as.formula(paste(Y_COL, "~", paste(vars, collapse = " + ")))
  
  # 5A. Hồi quy Logit với Cluster SE theo 'coin'
  # Gói fixest dùng feglm cực kỳ mạnh cho dữ liệu Panel
  fit <- feglm(fml, data = train_df, family = "logit", cluster = ~coin)
  
  # 5B. Kiểm tra đa cộng tuyến (VIF)
  vif_val <- car::vif(glm(fml, data = train_df, family = binomial))
  vif_df <- tibble(variable = names(vif_val), VIF = as.numeric(vif_val), model = name)
  
  # 5C. Tìm ngưỡng (Cutoff) tối ưu F1 trên tập Train
  p_train <- predict(fit, train_df, type = "response")
  grid <- seq(0.001, 0.2, length.out = 100)
  f1s <- map_dbl(grid, function(t) {
    preds <- as.numeric(p_train >= t)
    target <- train_df[[Y_COL]]
    prec <- sum(preds == 1 & target == 1) / (sum(preds == 1) + 1e-9)
    rec  <- sum(preds == 1 & target == 1) / (sum(target == 1) + 1e-9)
    return(2 * (prec * rec) / (prec + rec + 1e-9))
  })
  best_t <- grid[which.max(f1s)]
  
  # 5D. Đánh giá trên tập Test
  p_test <- predict(fit, test_df, type = "response")
  roc_obj <- roc(test_df[[Y_COL]], p_test, quiet = TRUE)
  
  metrics <- tibble(
    model      = name,
    pseudo_r2  = fit$sq.cor, # McFadden Pseudo-R2
    auc        = as.numeric(auc(roc_obj)),
    cutoff     = best_t,
    n_obs      = fit$nobs
  )
  
  # 5E. Bảng hệ số
  coefs <- as_tibble(coeftable(fit), rownames = "variable") %>%
    mutate(model = name, 
           sig = case_when(`Pr(>|z|)` < 0.01 ~ "***", 
                           `Pr(>|z|)` < 0.05 ~ "**", 
                           `Pr(>|z|)` < 0.1 ~ "*", 
                           TRUE ~ "ns"))
  
  return(list(coefs = coefs, metrics = metrics, vif = vif_df))
}

# 6. THỰC THI & LƯU KẾT QUẢ ----------------------------------------------------
results <- map2(specs, names(specs), run_model)

final_coefs   <- map_dfr(results, "coefs")
final_metrics <- map_dfr(results, "metrics")
final_vif     <- map_dfr(results, "vif")

# Xuất ra Excel nhiều Sheet
write_xlsx(list(Coefficients = final_coefs, Summary = final_metrics, VIF = final_vif), OUT_FILE)

message(paste("✅ HOÀN TẤT! Kết quả lưu tại:", OUT_FILE))
print(final_metrics)