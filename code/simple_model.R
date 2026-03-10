# ==============================================================================
# STABLECOIN DE-PEGGING LOGIT MODELS (R Version)
# Purpose : Chạy 5 mô hình hồi quy Logit dự báo rủi ro De-peg (Cluster SE)
# Input   : stablecoin_panel_full_features.csv
# Output  : model/simple_logit_results.xlsx
# ==============================================================================

# 1. KHAI BÁO THƯ VIỆN ---------------------------------------------------------
library(tidyverse) # Xử lý dữ liệu
library(fixest) # Hồi quy Logit với Cluster SE theo coin
library(pROC) # Tính toán AUC
library(writexl) # Xuất kết quả ra Excel
library(car) # Kiểm tra đa cộng tuyến (VIF)

# 2. CẤU HÌNH ĐƯỜNG DẪN --------------------------------------------------------
PANEL_FILE <- "data/processed/stablecoin_panel_full_features.csv"
OUTPUT_DIR <- "outputs"
OUT_FILE <- file.path(OUTPUT_DIR, "simple_logit_results.xlsx")

# Mốc thời gian Train/Test
TRAIN_START <- as.Date("2024-01-01")
TRAIN_END <- as.Date("2024-12-31")
TEST_START <- as.Date("2025-01-01")
TEST_END <- as.Date("2025-12-31")

Y_COL <- "depeg_static" # Biến phụ thuộc: De-peg (±1%)

# 3. CHUẨN BỊ MÔI TRƯỜNG & DỮ LIỆU ---------------------------------------------
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

panel <- read_csv(PANEL_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Tách tập Train và Test (Sửa lỗi 'train_df not found')
train_df <- panel %>% filter(date >= TRAIN_START & date <= TRAIN_END)
test_df <- panel %>% filter(date >= TEST_START & date <= TEST_END)

# In bảng phân phối label
message("[INFO] Phân phối nhãn Depeg trên tập Train:")
print(table(train_df$depeg_static, train_df$dummy))

# Centering biến liên tục dựa trên trung bình tập Train (tránh Data Leakage)
dev_mean <- mean(train_df$dev_abs_lag1, na.rm = TRUE)
sig_mean <- mean(train_df$sigma_dev_30d_lag1, na.rm = TRUE)

apply_prep <- function(df) {
  df %>% mutate(
    # Ép dummy về số (1 = Crypto, 0 = Fiat) để tính phép nhân Interaction
    dummy_num = ifelse(as.character(dummy) == "Crypto-backed" | as.numeric(dummy) == 1, 1, 0),

    # Tính Center variables
    dev_abs_c = dev_abs_lag1 - dev_mean,
    sigma_dev_30d_c = sigma_dev_30d_lag1 - sig_mean,

    # Tính biến tương tác (Interaction)
    dev_abs_c_x_dummy = dev_abs_c * dummy_num,
    sigma_dev_30d_c_x_dummy = sigma_dev_30d_c * dummy_num
  )
}

train_df <- apply_prep(train_df)
test_df <- apply_prep(test_df)

# 4. ĐỊNH NGHĨA 5 MÔ HÌNH (Bám sát file PDF) -----------------------------------
specs <- list(
  baseline = c("dev_abs_lag1", "sigma_dev_30d_lag1", "dummy"),
  interaction = c("dev_abs_c", "sigma_dev_30d_c", "dummy", "dev_abs_c_x_dummy", "sigma_dev_30d_c_x_dummy"),
  extended = c(
    "dev_abs_c", "sigma_dev_30d_c", "dummy", "dev_abs_c_x_dummy", "sigma_dev_30d_c_x_dummy",
    "BTC_Realized_Daily_Volatility_lag1", "global_mcap_logret_lag1", "fear_greed_index_lag1"
  ),
  dynamic = c(
    "illiq_lag1", "BTC_Realized_Daily_Volatility_lag1", "ETH_percent_change_24h_lag1",
    "global_mcap_logret_lag1", "fear_greed_index_lag1", "dummy"
  ),
  full_static = c(
    "dev_abs_c", "sigma_dev_30d_c", "log_volume_lag1",
    "BTC_Realized_Daily_Volatility_lag1", "ETH_percent_change_24h_lag1",
    "global_mcap_logret_lag1", "fear_greed_index_lag1", "dummy",
    "dev_abs_c_x_dummy", "sigma_dev_30d_c_x_dummy"
  )
)

# Load thêm thư viện vẽ đồ thị đánh giá
library(yardstick) # Tính toán PR Curve
library(ggplot2)

# 5. HÀM THỰC THI HỒI QUY (Đã fix lỗi trống bảng) -----------------------------
run_model <- function(vars, name) {
  message(">>> Đang chạy mô hình: ", name)

  fml <- as.formula(paste(Y_COL, "~", paste(vars, collapse = " + ")))

  # 5A. Hồi quy
  fit <- feglm(fml, data = train_df, family = "logit", cluster = ~coin)

  # 5B. Kiểm tra VIF
  vif_df <- tryCatch(
    {
      vif_val <- car::vif(glm(fml, data = train_df, family = binomial))
      tibble(variable = names(vif_val), VIF = as.numeric(vif_val), model = name)
    },
    error = function(e) tibble(variable = vars, VIF = NA_real_, model = name)
  )

  # 5C. Tính Cutoff trên Train
  p_train <- predict(fit, train_df, type = "response")
  grid <- seq(0.001, 0.2, length.out = 100)
  f1s <- map_dbl(grid, function(t) {
    preds <- as.numeric(p_train >= t)
    target <- train_df[[Y_COL]]
    prec <- sum(preds == 1 & target == 1) / (sum(preds == 1) + 1e-9)
    rec <- sum(preds == 1 & target == 1) / (sum(target == 1) + 1e-9)
    return(2 * (prec * rec) / (prec + rec + 1e-9))
  })
  best_t <- grid[which.max(f1s)]

  # 5D. Đánh giá trên Test (Sửa bẫy lỗi khi Test không có sự kiện Depeg)
  p_test <- predict(fit, newdata = test_df, type = "response")

  auc_val <- NA
  if (length(unique(test_df[[Y_COL]])) > 1) { # Phải có đủ 0 và 1 mới tính được AUC
    roc_obj <- roc(test_df[[Y_COL]], p_test, quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
  } else {
    message("    [Cảnh báo] Tập Test không có sự kiện depeg (toàn 0). Không tính được AUC.")
  }

  # Fix lỗi lấy Pseudo R2
  metrics <- tibble(
    model      = name,
    pseudo_r2  = ifelse(is.null(fit$pseudo_r2), NA, fit$pseudo_r2),
    auc        = auc_val,
    cutoff     = best_t,
    n_obs      = fit$nobs
  )

  # 5E. Hệ số
  coefs <- as_tibble(coeftable(fit), rownames = "variable") %>%
    mutate(
      model = name,
      Odds_Ratio = exp(Estimate),
      CI_Lower = exp(Estimate - 1.96 * `Std. Error`),
      CI_Upper = exp(Estimate + 1.96 * `Std. Error`),
      sig = case_when(
        `Pr(>|z|)` < 0.01 ~ "***",
        `Pr(>|z|)` < 0.05 ~ "**",
        `Pr(>|z|)` < 0.1 ~ "*",
        TRUE ~ "ns"
      )
    )

  # 5F. Lưu data dự đoán để vẽ PR Curve
  test_preds <- tibble(
    truth = as.factor(test_df[[Y_COL]]),
    prob = p_test,
    model = name
  )

  return(list(coefs = coefs, metrics = metrics, vif = vif_df, preds = test_preds))
}

# 6. THỰC THI & LƯU KẾT QUẢ ----------------------------------------------------
results <- map2(specs, names(specs), run_model)

final_coefs <- map_dfr(results, "coefs")
final_metrics <- map_dfr(results, "metrics")
final_vif <- map_dfr(results, "vif")
final_preds <- map_dfr(results, "preds")

write_xlsx(list(Coefficients = final_coefs, Summary = final_metrics, VIF = final_vif), OUT_FILE)
message("✅ HOÀN TẤT! Đã fix lỗi xuất Excel thành công.")
print(final_metrics)

# 7. VẼ ĐỒ THỊ -----------------------------------------------------------------
message(">>> Đang vẽ đồ thị...")
theme_set(theme_bw())

# --- Figure 5: PR Curve (Precision-Recall) ---
# Chỉ vẽ nếu tập Test có cả 2 nhãn
if (length(unique(final_preds$truth)) > 1) {
  pr_plot <- final_preds %>%
    group_by(model) %>%
    pr_curve(truth, prob, event_level = "second") %>%
    autoplot() +
    labs(
      title = "Figure 5: Precision-Recall Curve (PR-AUC)",
      subtitle = "So sánh hiệu năng dự báo Depeg trên tập Test (Dữ liệu mất cân bằng)",
      x = "Recall (Độ phủ)", y = "Precision (Độ chính xác)"
    ) +
    theme(legend.position = "bottom", text = element_text(size = 12))

  ggsave(file.path(OUTPUT_DIR, "Figure_5_PR_Curve.png"), pr_plot, width = 8, height = 6)
}

# --- Figure 6: Forest Plot (Odds Ratios) ---
# Lọc bỏ hệ số (Intercept) để đồ thị không bị lệch scale
forest_data <- final_coefs %>% filter(variable != "(Intercept)")

fig_forest <- ggplot(forest_data, aes(x = Odds_Ratio, y = variable, color = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper),
    position = position_dodge(width = 0.6), height = 0.3
  ) +
  # Chuyển x scale sang log10 để dễ nhìn vì Odds Ratio có thể rất lớn/nhỏ
  scale_x_log10() +
  labs(
    title = "Figure 6: Forest Plot của Odds Ratios",
    subtitle = "Vạch đỏ ở mốc 1 (OR > 1: tăng rủi ro depeg, OR < 1: giảm rủi ro). Trục X dạng Log10.",
    x = "Odds Ratio (Scale Log10)",
    y = "Biến số dự báo"
  ) +
  theme(legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "Figure_6_Forest_Plot.png"), fig_forest, width = 10, height = 7)
message("✅ Đã lưu Figure 5 & 6 vào thư mục model/.")
