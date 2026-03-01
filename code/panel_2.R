# ==============================================================================
# STABLECOIN PANEL - FULL FEATURE ENGINEERING (R Version)
# Purpose : Xây dựng bộ dữ liệu Panel hoàn chỉnh cho mô hình hồi quy Logit
# Input   : Các file raw (Fear & Greed, Market Cap, Yahoo Finance,...)
# Output  : stablecoin_panel_full_features.csv
# ==============================================================================

# 1. KHAI BÁO THƯ VIỆN ---------------------------------------------------------
library(tidyverse) # Xử lý bảng dữ liệu (thay cho pandas)
library(lubridate) # Xử lý thời gian
library(slider)    # Tính rolling stats (thay cho pandas rolling)

# 2. CẤU HÌNH ĐƯỜNG DẪN --------------------------------------------------------
# Tự động nhận diện cấu trúc thư mục của bạn
DATA_RAW_DIR <- "data/raw"
DATA_PROC_DIR <- "data/processed"

MASTER_FILE       <- file.path(DATA_PROC_DIR, "master_stablecoin_research_final.csv")
BTC_ETH_FILE      <- file.path(DATA_RAW_DIR,  "crypto_history_yfinance_clean.csv")
GLOBAL_MCAP_FILE  <- file.path(DATA_RAW_DIR,  "global_crypto_market_cap_daily_2024_2026.csv")
TOTAL_STABLE_FILE <- file.path(DATA_RAW_DIR,  "defillama_total_stablecoin_cap.csv")
FEAR_GREED_FILE   <- file.path(DATA_RAW_DIR,  "crypto_fear_and_greed_index.csv")
OUTPUT_FILE       <- file.path(DATA_PROC_DIR, "stablecoin_panel_full_features.csv")

# Các tham số mô hình
START_DATE <- as.Date("2024-01-01")
END_DATE   <- as.Date("2025-12-31")
FIAT_BACKED <- c("Tether USDt", "USDC", "PayPal USD", "First Digital USD")

# 3. HÀM BỔ TRỢ (HELPER FUNCTIONS) ---------------------------------------------
winsorize <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  case_when(x < q[1] ~ q[1], x > q[2] ~ q[2], TRUE ~ x)
}

# 4. STEP 1: LOAD MASTER PANEL -------------------------------------------------
message("STEP 1: Loading master stablecoin panel...")
if (!file.exists(MASTER_FILE)) stop("Không tìm thấy file Master!")

stable <- read_csv(MASTER_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  select(date, coin = Coin, close = Price, volume = Volume, 
         market_cap = Market_Cap, circ_supply = Supply, backing_type = Category) %>%
  arrange(coin, date)

# 5. STEP 2: STABLECOIN-LEVEL FEATURES -----------------------------------------
message("STEP 2: Computing stablecoin-level features...")
stable <- stable %>%
  group_by(coin) %>%
  mutate(
    log_ret = log(close / lag(close)),
    # Volatility indicators
    dev_abs = abs(close - 1),
    sigma_dev_30d = slide_dbl(log_ret, sd, .before = 29, .complete = FALSE),
    log_volume = log1p(volume),
    illiq = pmin(1 / log_volume, 2.0), # ILLIQ_CAP = 2.0
    # % Changes 7d
    circulating_supply_percent_change_7d = (circ_supply / lag(circ_supply, 7)) - 1
  ) %>%
  ungroup()

# 6. STEP 3: BTC / ETH SPILLOVER -----------------------------------------------
message("STEP 3: BTC / ETH spillover features...")
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
    select(date, 
           !!paste0(sym, "_Realized_Daily_Volatility") := vol,
           !!paste0(sym, "_percent_change_24h") := pct_24h)
}

btc_feat <- build_crypto(crypto, "BTC")
eth_feat <- build_crypto(crypto, "ETH")

# 7. STEP 4: MACRO & SENTIMENT -------------------------------------------------
message("STEP 4: Macro & sentiment indicators...")

# Global Market Cap
gmcap <- read_csv(GLOBAL_MCAP_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         global_mcap_logret = log(market_cap / lag(market_cap))) %>%
  select(date, global_mcap_logret)

# Fear & Greed
fgi <- read_csv(FEAR_GREED_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(timestamp)) %>%
  select(date, fear_greed_index = value)

# 8. STEP 5: MERGING & REFINING ------------------------------------------------
message("STEP 5: Merging all sources...")
panel <- stable %>%
  left_join(btc_feat, by = "date") %>%
  left_join(eth_feat, by = "date") %>%
  left_join(gmcap,    by = "date") %>%
  left_join(fgi,      by = "date") %>%
  arrange(coin, date) %>%
  group_by(coin) %>%
  fill(fear_greed_index, .direction = "down") %>%
  ungroup()

# Labels & Dummies
panel <- panel %>%
  mutate(
    depeg_static = if_else(dev_abs >= 0.01, 1, 0), # ±1.0% threshold
    dummy = if_else(coin %in% FIAT_BACKED, 0, 1)    # 0=Fiat, 1=Crypto
  )

# 9. STEP 6: WINSORIZATION & LAGS ----------------------------------------------
message("STEP 6: Winsorization & t-1 lag...")

# Winsorize selected columns
target_cols <- c("dev_abs", "sigma_dev_30d", "global_mcap_logret", "fear_greed_index")
panel <- panel %>%
  mutate(across(all_of(target_cols), ~winsorize(.x)))

# Create Lag1 (Quan trọng để tránh nội sinh - Endogeneity)
LAG_TARGETS <- c("dev_abs", "sigma_dev_30d", "log_volume", "illiq", 
                 "circulating_supply_percent_change_7d", "BTC_Realized_Daily_Volatility", 
                 "ETH_percent_change_24h", "global_mcap_logret", "fear_greed_index")

panel <- panel %>%
  group_by(coin) %>%
  mutate(across(all_of(LAG_TARGETS), list(lag1 = ~lag(.x)))) %>%
  ungroup()

# 10. STEP 7: SAVE DATA --------------------------------------------------------
panel_final <- panel %>%
  filter(date >= START_DATE & date <= END_DATE)

write_csv(panel_final, OUTPUT_FILE)
message(paste("✅ SUCCESS: Saved panel to", OUTPUT_FILE))
message(paste("📊 Depeg rate (static):", round(mean(panel_final$depeg_static, na.rm=T), 4)))