# ==============================================================================
# STABLECOIN DE-PEGGING LOGIT MODELS (R Version)
# Purpose : Chạy các mô hình Logit với 3 threshold de-peg (Kinh tế lượng)
# Input   : data/processed/stablecoin_panel_full_features.csv
# Output  : outputs/simple_logit_results.xlsx
# ==============================================================================

# ==============================================================================
# 1. LIBRARIES
# ==============================================================================
library(tidyverse)
library(writexl)
library(car)
library(ggplot2)

# Thư viện Kinh tế lượng
library(brglm2)            # Xử lý Separation bằng Firth's Penalized Likelihood
library(ResourceSelection) # Kiểm định Hosmer-Lemeshow
library(DescTools)         # Tính Pseudo R2 (McFadden)
library(lmtest)            # Trích xuất p-value từ Clustered SE
library(sandwich)          # Tính Clustered Standard Errors cho Panel Data

# ==============================================================================
# 2. PATHS & CONFIG
# ==============================================================================
PANEL_FILE <- "data/processed/stablecoin_panel_full_features.csv"
OUTPUT_DIR <- "outputs"
OUT_FILE   <- file.path(OUTPUT_DIR, "simple_logit_results.xlsx")

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# DÙNG FULL SAMPLE
SAMPLE_START <- as.Date("2019-01-01")
SAMPLE_END   <- as.Date("2025-12-31")
LABELS <- c("depeg_005", "depeg_01", "depeg_0075")

# ------------------------------------------------------------------------------
# 3. HELPERS
# ------------------------------------------------------------------------------
winsorize_vec <- function(x, low, high) {
  pmin(pmax(x, low), high)
}

fit_winsor_bounds <- function(df, cols, p_low = 0.01, p_high = 0.99) {
  tibble(
    variable = cols,
    low  = purrr::map_dbl(cols, ~quantile(df[[.x]], p_low,  na.rm = TRUE)),
    high = purrr::map_dbl(cols, ~quantile(df[[.x]], p_high, na.rm = TRUE))
  )
}

apply_winsor_bounds <- function(df, bounds) {
  out <- df
  for (i in seq_len(nrow(bounds))) {
    v <- bounds$variable[i]
    out[[v]] <- winsorize_vec(out[[v]], bounds$low[i], bounds$high[i])
  }
  out
}

get_vif_table <- function(fml, df, model_name, threshold_name) {
  out <- tryCatch({
    v <- car::vif(glm(fml, data = df, family = binomial))
    if (is.matrix(v)) {
      tibble(
        variable = rownames(v),
        VIF = as.numeric(v[, ncol(v)]),
        model = model_name,
        threshold = threshold_name
      )
    } else {
      tibble(
        variable = names(v),
        VIF = as.numeric(v),
        model = model_name,
        threshold = threshold_name
      )
    }
  }, error = function(e) {
    tibble(
      variable = all.vars(fml)[-1],
      VIF = NA_real_,
      model = model_name,
      threshold = threshold_name
    )
  })
  out
}

apply_prep <- function(df, sig_mean) {
  df %>%
    mutate(
      # Tạo biến Dummy: 1 nếu hôm qua lệch trên 0.5%, 0 nếu không
      depeg_yesterday = if_else(dev_abs_lag1 >= 0.005, 1L, 0L),
      sigma_dev_30d_c = sigma_dev_30d_lag1 - sig_mean,
      sigma_dev_30d_c_x_dummy = sigma_dev_30d_c * dummy_crypto
    )
}

# ------------------------------------------------------------------------------
# 4. MODEL SPECS & CONFIG
# ------------------------------------------------------------------------------
specs <- list(
  m1_core = c(
    "depeg_yesterday",
    "sigma_dev_30d_lag1",
    "dummy_crypto"
  ),
  m2_interaction = c(
    "depeg_yesterday",
    "sigma_dev_30d_c",
    "dummy_crypto",
    "sigma_dev_30d_c_x_dummy"
  ),
  m3_full = c(
    "depeg_yesterday",
    "sigma_dev_30d_c",
    "dummy_crypto",
    "sigma_dev_30d_c_x_dummy",
    "log_volume_lag1",
    "BTC_Realized_Daily_Volatility_lag1",
    "ETH_percent_change_24h_lag1",
    "stable_mcap_logret_lag1",
    "fear_greed_index_lag1"
  )
)

usable_map <- c(
  m1_core = "usable_m1",
  m2_interaction = "usable_m2",
  m3_full = "usable_m5"
)

WIN_COLS <- c(
  "dev_abs_lag1",
  "sigma_dev_30d_lag1",
  "stable_mcap_logret_lag1",
  "fear_greed_index_lag1"
)

# ------------------------------------------------------------------------------
# 5. LOAD & PREP PANEL DATA (FIXED)
# ------------------------------------------------------------------------------
panel <- read_csv(PANEL_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Thực hiện Winsorize để tránh Outliers
bounds <- fit_winsor_bounds(panel, WIN_COLS)
panel  <- apply_winsor_bounds(panel, bounds)

# Thực hiện Centering (Trừ đi trung bình) và tạo Interaction terms
# dev_mean <- mean(panel$dev_abs_lag1, na.rm = TRUE)
sig_mean <- mean(panel$sigma_dev_30d_lag1, na.rm = TRUE)
panel    <- apply_prep(panel, sig_mean)

# ------------------------------------------------------------------------------
# 6. RUN ONE MODEL
# ------------------------------------------------------------------------------
run_one_model <- function(features, full_df, model_name, label_name) {
  df_fit <- full_df %>% filter(!is.na(y))
  
  # Tạo công thức mô hình chuẩn (Formula) từ mảng features
  fml <- as.formula(paste("y ~", paste(features, collapse = " + ")))
  
  # 1. FIT MODEL: Dùng Firth's Penalty (brglmFit) để trị Separation
  fit <- tryCatch({
    glm(fml, data = df_fit, family = binomial(link = "logit"), method = "brglmFit")
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    metrics <- tibble(
      threshold = label_name, model = model_name, 
      status = "fit_error", note = fit$message
    )
    return(list(coefs = tibble(), metrics = metrics, vif = tibble()))
  }

  # 2. TÍNH VIF (Đa cộng tuyến)
  vif_df <- get_vif_table(fml, df_fit, model_name, label_name)

  # 3. KIỂM ĐỊNH MÔ HÌNH (Goodness of Fit)
  p_hat <- predict(fit, type = "response")
  
  brier_score <- mean((p_hat - df_fit$y)^2, na.rm = TRUE)
  
  # Tính AUC để thay thế HL
  auc_val <- tryCatch({
    as.numeric(pROC::auc(pROC::roc(df_fit$y, p_hat, quiet = TRUE)))
  }, error = function(e) NA_real_)

  hl_test <- tryCatch({
    ResourceSelection::hoslem.test(df_fit$y, p_hat, g = 10)
  }, error = function(e) list(p.value = NA_real_))
  
  ps_r2 <- tryCatch({ DescTools::PseudoR2(fit, which = "McFadden") }, error = function(e) NA_real_)

  metrics <- tibble(
    threshold = label_name,
    model = model_name,
    n_obs = nobs(fit),
    event_count = sum(df_fit$y == 1, na.rm = TRUE),
    event_rate = mean(df_fit$y, na.rm = TRUE),
    pseudo_r2 = ps_r2,
    hosmer_lemeshow_pval = hl_test$p.value,
    brier_score = brier_score,
    auc = auc_val,
    status = "ok",
    note = "Clustered SE (Coin) + Firth Penalty Applied"
  )

  # 4. TÍNH CLUSTERED STANDARD ERRORS (Cho Panel Data)
  clustered_se <- tryCatch({
    lmtest::coeftest(fit, vcov = sandwich::vcovCL, cluster = ~coin)
  }, error = function(e) {
    # Fallback nếu lỗi (hiếm khi xảy ra)
    summary(fit)$coefficients
  })
  
  coefs <- as_tibble(clustered_se[,], rownames = "variable") %>%
    rename(
      Estimate = Estimate,
      `Std. Error` = `Std. Error`,
      `z value` = `z value`,
      `Pr(>|z|)` = `Pr(>|z|)`
    ) %>%
    mutate(
      threshold = label_name,
      model = model_name,
      Odds_Ratio = exp(Estimate),
      CI_Lower = exp(Estimate - 1.96 * `Std. Error`),
      CI_Upper = exp(Estimate + 1.96 * `Std. Error`),
      sig = case_when(
        `Pr(>|z|)` < 0.01 ~ "***",
        `Pr(>|z|)` < 0.05 ~ "**",
        `Pr(>|z|)` < 0.10 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    relocate(threshold, model, variable, Estimate, Odds_Ratio, CI_Lower, CI_Upper, sig)

  return(list(coefs = coefs, metrics = metrics, vif = vif_df))
}

# ------------------------------------------------------------------------------
# 7. VÒNG LẶP CHÍNH (MAIN EXECUTION)
# ------------------------------------------------------------------------------
all_coefs   <- list()
all_metrics <- list()
all_vifs    <- list()

for (lab in LABELS) {
  cat("\n=========================================\n")
  cat("THRESHOLD:", lab, "\n")
  
  # Lấy dữ liệu từ panel (FIXED: df -> panel)
  full_df <- panel %>%
    filter(date >= SAMPLE_START & date <= SAMPLE_END) %>%
    mutate(y = !!sym(lab))
    
  for (m_name in names(specs)) {
    cat("  -> Running", m_name, "...\n")
    
    # Sử dụng usable_map để lấy đúng tên cột (FIXED logic nối chuỗi)
    usable_col <- usable_map[[m_name]]
    model_df <- full_df %>% filter(!!sym(usable_col) == 1L)
    
    res <- run_one_model(specs[[m_name]], model_df, m_name, lab)
    
    all_coefs[[length(all_coefs) + 1]]     <- res$coefs
    all_metrics[[length(all_metrics) + 1]] <- res$metrics
    all_vifs[[length(all_vifs) + 1]]       <- res$vif
  }
}

final_coefs   <- bind_rows(all_coefs)
final_metrics <- bind_rows(all_metrics)
final_vifs    <- bind_rows(all_vifs)

# ------------------------------------------------------------------------------
# 8. XUẤT KẾT QUẢ KINH TẾ LƯỢNG VÀO EXCEL
# ------------------------------------------------------------------------------
writexl::write_xlsx(
  list(
    Overview = final_metrics,
    Coefficients = final_coefs,
    VIF = final_vifs
  ),
  path = OUT_FILE
)
cat("\n=> Đã xuất báo cáo Kinh tế lượng thành công tại:", OUT_FILE, "\n")

# ------------------------------------------------------------------------------
# 9. OPTIONAL PLOTS
# ------------------------------------------------------------------------------
theme_set(theme_bw())

# Chỉ giữ lại Forest plot, bỏ biểu đồ Machine Learning PR Curve
for (lab in LABELS) {
  forest_data <- final_coefs %>%
    filter(threshold == lab, variable != "(Intercept)")

  p <- ggplot(forest_data, aes(x = Odds_Ratio, y = variable, color = model)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.7) +
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbarh(
      aes(xmin = CI_Lower, xmax = CI_Upper),
      position = position_dodge(width = 0.6),
      height = 0.25
    ) +
    scale_x_log10() +
    labs(
      title = paste("Forest Plot -", lab),
      x = "Odds Ratio (log scale)",
      y = "Variables"
    ) +
    theme(legend.position = "right")

  ggsave(
    file.path(OUTPUT_DIR, paste0("Forest_", lab, ".png")),
    p, width = 10, height = 7
  )
}

message("✅ Hoàn thành toàn bộ quy trình Kinh tế lượng.")
# ==============================================================================
# 7. BỔ SUNG: MARGINAL EFFECTS & PLOT CHO MÔ HÌNH CHÍNH (m3_full, depeg_01)
# ==============================================================================
# Nếu chưa cài thư viện, hãy bỏ comment dòng dưới đây để cài:
# install.packages("marginaleffects")
library(marginaleffects)

cat("\n--- Đang tính toán Marginal Effects cho Main Model (m3_full - Ngưỡng 1%) ---\n")

# 1. Chuẩn bị lại data cho mốc 1% (đảm bảo không bị nhiễu NA)
df_main <- panel %>% 
  filter(!is.na(depeg_01), !is.na(dummy_crypto), !is.na(sigma_dev_30d_c)) %>%
  mutate(y = depeg_01)

# 2. Fit lại mô hình m3_full bằng đúng phương pháp của mô hình gốc (Firth's Penalty)
main_model_fit <- glm(
  as.formula(paste("y ~", paste(specs[["m3_full"]], collapse = " + "))),
  data = df_main,
  family = binomial(link = "logit"),
  method = "brglmFit"
)

# 3. Tính Average Marginal Effects (AME)
# Dùng vcov = ~coin để tính toán dải sai số theo Clustered SE (Đồng bộ với sandwich)
mfx_summary <- avg_slopes(main_model_fit, vcov = ~coin)

# In ra console để xem nhanh
print(mfx_summary)

# Lưu kết quả tính toán Marginal Effect ra file CSV
write_csv(
  as.data.frame(mfx_summary), 
  file.path(OUTPUT_DIR, "Marginal_Effects_m3_full_01.csv")
)

# 4. VẼ BIỂU ĐỒ TƯƠNG TÁC CHO RQ2 (Interaction Effect)
# Đồ thị thể hiện: Độ biến động thị trường tác động đến rủi ro De-peg như thế nào,
# Truyền vcov = ~coin để khoảng tin cậy (vùng xám) trên đồ thị được tính đúng
p_interaction <- plot_predictions(
  main_model_fit, 
  condition = c("sigma_dev_30d_c", "dummy_crypto"),
  vcov = ~coin
) +
  labs(
    title = "Xác suất rủi ro De-peg (Ngưỡng 1%) theo cơ chế bảo chứng",
    subtitle = "Hiệu ứng tương tác giữa Độ biến động nội tại và Loại Stablecoin",
    x = "Độ biến động giá 30 ngày trước (Centered)",
    y = "Xác suất dự báo De-peg (Predicted Probability)",
    color = "Cơ chế bảo chứng",
    fill = "Cơ chế bảo chứng"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#d62728"),
    labels = c("0" = "Fiat-backed", "1" = "Crypto-backed/Algo")
  ) +
  scale_fill_manual(
    values = c("0" = "#1f77b4", "1" = "#1ed8d89f"),
    labels = c("0" = "Fiat-backed", "1" = "Crypto-backed/Algo")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

# Lưu biểu đồ
ggsave(
  file.path(OUTPUT_DIR, "Interaction_Effect_RQ2.png"), 
  p_interaction, 
  width = 8, height = 6, dpi = 300
)

cat("✅ Đã hoàn thành! Vui lòng kiểm tra file 'Marginal_Effects_m3_full_01.csv' và biểu đồ 'Interaction_Effect_RQ2.png' trong thư mục outputs.\n")