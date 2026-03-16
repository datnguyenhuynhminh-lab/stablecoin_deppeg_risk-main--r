# Stablecoin De-Peg Risk Analysis (2019–2025)

## 1. Project Title & Overview

**Stablecoin De-Peg Risk Analysis** is an empirical econometric study of **rare de-peg events** across 12 major stablecoins using **daily panel data (2019–2025)**. The primary objective is to **model and forecast de-peg risk**—defined as a deviation from the USD peg—while rigorously testing whether **Fiat-backed stablecoins are structurally safer than crypto/algorithmic counterparts under market stress**.

This repository implements a reproducible pipeline from data collection to econometric estimation, emphasizing robust inference in the presence of **rare events, separation, and within-entity serial dependence**.

---

## 2. Key Findings

- **Bank-run mechanism dominates.** De-peg persistence (`lag(depeg_01)`) and liquidity proxies (`volume`) are the strongest predictors of future de-pegs. Macro shocks (e.g., BTC volatility) are secondary and largely absorbed by micro-level structural weaknesses.

- **Fiat-backed myth debunked.** During severe crises (defined as de-peg > 1%), **Fiat-backed stablecoins do not exhibit a statistically significant safety advantage** over crypto/algorithmic-backed ones. The market prices **liquidity depth**, not the collateral label.

---

## 3. Methodology

### 3.1 Outcome Definition
- **De-peg indicator (binary):** `depeg_01` = 1 if |price − 1| ≥ 0.01 (i.e., ≥1% deviation), else 0.
- Event rate ≈ **12.5%** (rare-event regime).

### 3.2 Econometric Model
- **Panel Logistic Regression** with fixed effects / entity clustering.
- Estimation uses **Firth’s penalized likelihood** via `brglm2` to address **quasi-separation and coefficient explosion** typical of rare events.

\[ \Pr(y_{it}=1 \,|\, X_{it}) = \frac{1}{1 + \exp(-X_{it}\beta)} \]

- **Clustered standard errors** (entity-level) are computed using the `sandwich` package to correct for **serial correlation within coins**.

### 3.3 Interpretation
- Report **Average Marginal Effects (AMEs)** instead of raw log-odds coefficients for **actionable economic interpretation**.
- AMEs are computed to quantify the change in de-peg probability for a unit change in predictors, averaged over the sample.

---

## 4. Data Pipeline & Preprocessing

### 4.1 Data Sources
- Stablecoin prices and on-chain metrics (multiple APIs / crawl scripts).

### 4.2 Preprocessing Notes (Key Technical Details)
- **Lagging:** All independent variables are lagged by 1 day (`t-1`) to prevent forward-looking leakage.
- **De-peg threshold:** Binary event defined at ±1% deviation from USD peg (`depeg_01`).
- **Winsorization:** Outliers in **volume** and **volatility** are winsorized at the 1st and 99th percentiles. Winsorization is fitted on the training set to avoid look-ahead bias.
- **Collinearity:** Interaction terms introduce structural collinearity (VIF ≈ 19). This is **expected and retained** because it arises from theoretically motivated interaction constructs.

---

## 5. Repository Structure

```
stablecoin_deppeg_risk-main--r/
├── code/                     # R scripts (data wrangling + modeling)
│   ├── collect_data.R        # Data ingestion + merge + initial cleaning
│   ├── panel_2.R             # Feature engineering (lags, volatility, interactions)
│   ├── simple_model.R        # Model estimation + clustered SE + marginal effects
│   └── crawl_*               # Data collection scripts (APIs / web sources)
├── data/
│   ├── raw/                  # Raw input CSVs
│   ├── processed/            # Cleaned master panel datasets
│   └── output/               # Derived CSV outputs (model tables, predictions)
├── outputs/                  # Figures, charts, and stats tables (CSV/PNG)
├── model/                    # Exported model results (Excel, tables)
├── renv/                     # R environment cache
├── renv.lock                 # Locked package versions
└── README.md                 # This documentation
```

---

## 6. Reproducibility Guide (How to Run)

1. **Clone repository**
   ```sh
   git clone <repo-url>
   cd stablecoin_deppeg_risk-main--r
   ```

2. **Restore R environment**
   In R (RStudio or terminal):
   ```r
   renv::restore()
   ```

3. **Prepare the data**
   ```r
   source("code/collect_data.R")
   source("code/panel_2.R")
   ```

4. **Estimate the model & produce outputs**
   ```r
   source("code/simple_model.R")
   ```

5. **Inspect results**
   - **CSV outputs:** `data/processed/` and `outputs/` (marginal effects, coefficient tables)
   - **Figures:** `outputs/` (plots showing risk dynamics)

> ⚠️ Ensure the data pipeline produces the same panel alignment and lag structure before interpreting marginal effects. This pipeline is designed for **reproducible inference** in a rare-event panel setting.

---

## 7. Dependencies

Core R packages used in this project:

- `dplyr`
- `brglm2` (Firth penalized logit)
- `sandwich` (clustered SE)
- `lmtest` (coefficient inference via `coeftest`)
- `marginaleffects` (AME computations)
- `ggplot2` (plots)
- `lubridate` (date handling)

> ✅ Use `renv::snapshot()` after installing additional packages to lock dependencies.

---

## 8. Authors / Citation

**Author:** Quyền P.M., Đạt N.H.M. (2026)


> “Stablecoin De-Peg Risk Analysis: A Panel Logit Study with Firth Penalization and Clustered Standard Errors.”

---

*Note:* This document is intended to support **reproducibility** and **academic transparency** for researchers studying stablecoin de-peg risk in a panel framework.
