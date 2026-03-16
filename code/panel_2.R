# ==============================================================================
# STABLECOIN PANEL - FULL FEATURE ENGINEERING (CẬP NHẬT 2019-2026 & DEFILLAMA)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(slider)

# 1. CẤU HÌNH ĐƯỜNG DẪN TƯƠNG ĐỐI VÀ TUYỆT ĐỐI
DATA_RAW_DIR  <- "data/raw"
DATA_PROC_DIR <- "data/processed"

MASTER_FILE      <- file.path(DATA_PROC_DIR, "master_stablecoin_research_final.csv")
BTC_ETH_FILE     <- file.path(DATA_RAW_DIR,  "crypto_history_yfinance_clean.csv")
FEAR_GREED_FILE  <- file.path(DATA_RAW_DIR,  "crypto_fear_and_greed_index.csv")
OUTPUT_FILE      <- file.path(DATA_PROC_DIR, "stablecoin_panel_full_features.csv")

# ĐƯỜNG DẪN MỚI CHO FILE DEFILLAMA STABLECOIN MARKET CAP
STABLE_MCAP_FILE <- "D:\\SCHOOL\\HK5 (25-26)\\GOI_2\\stablecoin_deppeg_risk-main--r\\data\\raw\\defillama_total_stablecoin_cap.csv"

# 2. CẬP NHẬT KHUNG THỜI GIAN
START_DATE <- as.Date("2019-01-01") 
END_DATE   <- as.Date("2025-12-31")

# ------------------------------------------------------------------------------
# 1) Load master & Mapping các cột đã tính toán sẵn
# ------------------------------------------------------------------------------
stable <- read_csv(MASTER_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  select(
    date,
    coin = Coin,
    close = Price,
    volume = Volume,
    market_cap = Market_Cap,
    circ_supply = Supply,
    backing_type = Category,
    log_ret = Log_Return,
    sigma_dev_30d = Volatility_30d,
    dev_abs = Peg_Deviation 
  ) %>%
  arrange(coin, date)

# ------------------------------------------------------------------------------
# 2) Stablecoin-level features
# ------------------------------------------------------------------------------
stable <- stable %>%
  group_by(coin) %>%
  arrange(date, .by_group = TRUE) %>%
  tidyr::fill(circ_supply, .direction = "downup") %>%
  mutate(
    log_volume = log1p(volume),
    illiq = pmin(1 / log_volume, 2.0)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 3) BTC / ETH spillover
# ------------------------------------------------------------------------------
crypto <- read_csv(BTC_ETH_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

build_crypto <- function(df, sym) {
  df %>%
    filter(symbol == sym) %>%
    arrange(date) %>%
    mutate(
      log_ret = log(price / lag(price)),
      vol = slide_dbl(log_ret, sd, .before = 4, .complete = FALSE),
      pct_24h = (price / lag(price)) - 1
    ) %>%
    select(
      date,
      !!paste0(sym, "_Realized_Daily_Volatility") := vol,
      !!paste0(sym, "_percent_change_24h") := pct_24h
    )
}

btc_feat <- build_crypto(crypto, "BTC")
eth_feat <- build_crypto(crypto, "ETH")

# ------------------------------------------------------------------------------
# 4) Macro & sentiment (CẬP NHẬT DEFILLAMA)
# ------------------------------------------------------------------------------
# Đọc file DefiLlama và tính % thay đổi vốn hóa hệ sinh thái Stablecoin
smcap <- read_csv(STABLE_MCAP_FILE, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(Date), # Tên cột gốc là Date viết hoa
    stable_mcap_logret = log(Total_Stable_Cap / lag(Total_Stable_Cap))
  ) %>%
  select(date, stable_mcap_logret)

fgi <- read_csv(FEAR_GREED_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(timestamp)) %>%
  select(date, fear_greed_index = value) %>%
  arrange(date) %>%
  tidyr::complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(fear_greed_index, .direction = "downup")

# ------------------------------------------------------------------------------
# 5) Merge
# ------------------------------------------------------------------------------
panel <- stable %>%
  left_join(btc_feat, by = "date") %>%
  left_join(eth_feat, by = "date") %>%
  left_join(smcap, by = "date") %>% # Đổi tên dataframe join thành smcap
  left_join(fgi, by = "date") %>%
  arrange(coin, date)

# ------------------------------------------------------------------------------
# 6) Labels + Backing Dummy
# ------------------------------------------------------------------------------
panel <- panel %>%
  mutate(
    dummy_crypto = if_else(backing_type != "Fiat-backed", 1L, 0L),
    depeg_005 = if_else(dev_abs >= 0.005, 1L, 0L),
    depeg_0075 = if_else(dev_abs >= 0.0075, 1L, 0L),
    depeg_01  = if_else(dev_abs >= 0.01,  1L, 0L),
    depeg_static = depeg_005 
  )

# ------------------------------------------------------------------------------
# 7) Lag 1
# ------------------------------------------------------------------------------
LAG_TARGETS <- c(
  "dev_abs",
  "sigma_dev_30d",
  "log_volume",
  "illiq",
  "BTC_Realized_Daily_Volatility",
  "ETH_percent_change_24h",
  "stable_mcap_logret", # Đã cập nhật tên biến mới
  "fear_greed_index"
)

panel <- panel %>%
  group_by(coin) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(across(all_of(LAG_TARGETS), ~lag(.x), .names = "{.col}_lag1")) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 8) Usable flags by model (Cập nhật tên biến stable_mcap_logret)
# ------------------------------------------------------------------------------
panel <- panel %>%
  mutate(
    usable_m1 = if_else(
      !is.na(depeg_static) &
      !is.na(dev_abs_lag1) &
      !is.na(sigma_dev_30d_lag1) &
      !is.na(dummy_crypto),
      1L, 0L
    ),
    usable_m2 = if_else(usable_m1 == 1L, 1L, 0L),
    usable_m3 = if_else(
      usable_m2 == 1L &
      !is.na(BTC_Realized_Daily_Volatility_lag1) &
      !is.na(stable_mcap_logret_lag1) &
      !is.na(fear_greed_index_lag1),
      1L, 0L
    ),
    usable_m4 = if_else(
      !is.na(depeg_static) &
      !is.na(illiq_lag1) &
      !is.na(BTC_Realized_Daily_Volatility_lag1) &
      !is.na(ETH_percent_change_24h_lag1) &
      !is.na(stable_mcap_logret_lag1) &
      !is.na(fear_greed_index_lag1) &
      !is.na(dummy_crypto),
      1L, 0L
    ),
    usable_m5 = if_else(
      !is.na(depeg_static) &
      !is.na(dev_abs_lag1) &
      !is.na(sigma_dev_30d_lag1) &
      !is.na(log_volume_lag1) &
      !is.na(BTC_Realized_Daily_Volatility_lag1) &
      !is.na(ETH_percent_change_24h_lag1) &
      !is.na(stable_mcap_logret_lag1) &
      !is.na(fear_greed_index_lag1) &
      !is.na(dummy_crypto),
      1L, 0L
    )
  )

# ------------------------------------------------------------------------------
# 9) Filter sample window
# ------------------------------------------------------------------------------
panel_final <- panel %>%
  filter(date >= START_DATE & date <= END_DATE)

# ------------------------------------------------------------------------------
# 10) Missing audit & Sample audit
# ------------------------------------------------------------------------------
missing_audit <- panel_final %>%
  summarise(
    across(
      c(
        dev_abs, sigma_dev_30d, log_volume, illiq,
        BTC_Realized_Daily_Volatility, ETH_percent_change_24h,
        stable_mcap_logret, fear_greed_index,
        dev_abs_lag1, sigma_dev_30d_lag1, log_volume_lag1, illiq_lag1,
        BTC_Realized_Daily_Volatility_lag1, ETH_percent_change_24h_lag1,
        stable_mcap_logret_lag1, fear_greed_index_lag1
      ),
      ~sum(is.na(.)),
      .names = "na_{.col}"
    )
  )

sample_audit <- panel_final %>%
  summarise(
    n_total = n(),
    n_depeg_005 = sum(depeg_005, na.rm = TRUE),
    n_depeg_0075 = sum(depeg_0075, na.rm = TRUE),
    n_depeg_01  = sum(depeg_01,  na.rm = TRUE),
    m1 = sum(usable_m1, na.rm = TRUE),
    m2 = sum(usable_m2, na.rm = TRUE),
    m3 = sum(usable_m3, na.rm = TRUE),
    m4 = sum(usable_m4, na.rm = TRUE),
    m5 = sum(usable_m5, na.rm = TRUE)
  )

print(t(missing_audit)) 
print(sample_audit)

# ------------------------------------------------------------------------------
# 11) Save
# ------------------------------------------------------------------------------
write_csv(panel_final, OUTPUT_FILE)
message(paste("Saved:", OUTPUT_FILE))
message(paste("Depeg rate 0.5% (Main):", round(mean(panel_final$depeg_005, na.rm = TRUE), 4)))
message(paste("Depeg rate 0.75%:", round(mean(panel_final$depeg_0075, na.rm = TRUE), 4)))
message(paste("Depeg rate 1.0%:", round(mean(panel_final$depeg_01,  na.rm = TRUE), 4)))