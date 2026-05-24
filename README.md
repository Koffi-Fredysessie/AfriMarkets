<div align="center">

<img src="https://raw.githubusercontent.com/Koffi-Fredysessie/AfriMarkets/main/man/figures/logo.png" width="160px" alt="AfriMarkets logo"/>

# AfriMarkets

### Access · Analyze · Understand African Capital Markets

[![R ≥ 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3?logo=r&logoColor=white)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-1.0.5-blue.svg)]()
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![GitHub issues](https://img.shields.io/github/issues/Koffi-Fredysessie/AfriMarkets)](https://github.com/Koffi-Fredysessie/AfriMarkets/issues)

**AfriMarkets** is an R package that provides unified, programmatic access to
historical and real-time financial data from African stock exchanges.  
Built for researchers, data scientists, analysts, and developers working on
African capital markets.

[Installation](#-installation) · [Quick Start](#-quick-start) · [Markets](#-supported-markets) · [Functions](#-function-reference) · [Contributing](#-contributing)

</div>

---

## 🌍 Why AfriMarkets?

African financial markets are rich with opportunity yet remain largely
inaccessible to the global data-science community.  
**AfriMarkets** bridges that gap with:

- A **single, consistent API** across heterogeneous exchanges and data sources
- Ready-to-use **OHLCV data** (Open, High, Low, Close, Volume)
- Built-in **statistical and financial analysis** tools
- **Interactive charts** powered by `highcharter`

---

## ✨ Features

| | Feature | Description |
|---|---|---|
| 📊 | Ticker discovery | Browse all available indexes and shares per market |
| 📈 | Historical data | Fetch daily OHLCV data across any custom date range |
| 🔄 | Flexible output | Long format, wide format, or both at once |
| 🕯️ | Interactive charts | Candlestick & multi-ticker line charts via `highcharter` |
| 💹 | Performance KPIs | Return, volatility, Sharpe ratio, max drawdown, and more |
| 🧪 | Statistical tests | Normality and stationarity test batteries |
| ⚡ | Simple API | Consistent function signatures across all markets |

---

## 📦 Installation

```r
# Install the development version from GitHub
# install.packages("devtools")
devtools::install_github("Koffi-Fredysessie/AfriMarkets")
```

> **Note:** An active internet connection is required at install time to
> resolve dependencies, and at runtime to fetch market data.

---

## 🔑 Supported Markets

| Flag | Code | Exchange | Region | Backend |
|---|---|---|---|---|
| 🌍 | `BRVM` | Bourse Régionale des Valeurs Mobilières | UEMOA (8 countries) | Custom scraper |
| 🇬🇭 | `GSE` | Ghana Stock Exchange | Ghana | Custom scraper |
| 🇳🇬 | `NGX` | Nigerian Exchange Group | Nigeria | Investing.com API |
| 🇿🇦 | `JSE` | Johannesburg Stock Exchange | South Africa | Investing.com API |
| 🇲🇦 | `MSE` | Casablanca Stock Exchange | Morocco | Investing.com API |
| 🇪🇬 | `EGX` | Egyptian Exchange | Egypt | Investing.com API |
| 🇹🇳 | `TSE` | Tunis Stock Exchange | Tunisia | Investing.com API |

> 🔜 **Coming soon:** Kenya NSE · Botswana BSE · Mauritius SEM

---

## 📌 Quick Start

### 1 · Load the package

```r
library(AfriMarkets)
```

---

### 2 · Discover available tickers

```r
# Single market — returns an S4 african_market object
brvm <- GET_tickers("BRVM")
brvm                      # formatted console summary
brvm@Shares               # data.frame of all shares
brvm@Indexes              # data.frame of all indexes
brvm@ListShares           # character vector of share codes
brvm@ListIndexes          # character vector of index codes

# All supported markets at once
all_markets <- GET_tickers("ALL")
names(all_markets)        # "BRVM" "GSE" "NGX" "JSE" "MSE" "EGX" "TSE"
all_markets$NGX@Shares
```

---

### 3 · Download historical OHLCV data

```r
# Single ticker — last 90 days (default)
df <- GET_data("BRVM", ticker = "BICC")

# Multiple tickers — custom date range
df <- GET_data(
  market_code = "BRVM",
  ticker      = c("BICC", "BOAB", "SNTS"),
  from        = "2023-01-01",
  to          = Sys.Date()
)

# All shares — wide format
df_wide <- GET_data("NGX", ticker = "ALL SHARES", output_format = "by_row")

# All instruments — both formats at once
result <- GET_data("JSE", ticker = "ALL", output_format = "all")
result$by_col   # long format data.frame
result$by_row   # wide format data.frame
```

**Special `ticker` keywords:**

| Keyword | Returns |
|---|---|
| `"ALL"` | All instruments (shares + indexes) |
| `"ALL SHARES"` | Equities only |
| `"ALL INDEXES"` | Indexes only |

---

### 4 · Plot interactive charts

```r
# Single ticker — candlestick + colour-coded volume bars
pplot(market = "BRVM", stock = "BICC", from = "2023-01-01")

# Multiple tickers simultaneously — grouped line chart
pplot(
  market = "BRVM",
  stock = c("BICC", "BOAB", "SNTS"),
  from = "2023-01-01",
  to   = Sys.Date()
)

# From a data.frame directly
pplot(market = df, up.col = "darkgreen", down.col = "red")
```

---

### 5 · Run statistical tests

```r
prices <- df$Close

# Normality tests (Shapiro-Wilk, Jarque-Bera, etc.)
normality_test(prices)

# All stationarity tests at once
stationarity_test(prices, type.test = "ALL")

# Specific tests
stationarity_test(
  prices,
  type.test = c(
    "augmented dickey-fuller test (adf)",
    "phillips-perron unit root test"
  )
)
```

---

## 🔄 Output Formats

| `output_format` | Structure | Best for |
|---|---|---|
| `"by_col"` *(default)* | Long / tidy — one row per date per ticker | `dplyr` pipelines · time-series models · `ggplot2` |
| `"by_row"` | Wide — tickers as columns | Correlation matrices · portfolio optimisation |
| `"all"` | Named list with both formats | Exploratory analysis |

**Long format column reference:**

| Column | Type | Description |
|---|---|---|
| `Date` | `Date` | Trading date |
| `Ticker` | `character` | Asset ticker symbol |
| `Open` | `numeric` | Opening price |
| `High` | `numeric` | Intraday high |
| `Low` | `numeric` | Intraday low |
| `Close` | `numeric` | Closing price |
| `Volume` | `numeric` | Trading volume |

---

## 💹 Performance Indicators

AfriMarkets provides built-in financial KPIs computed over any selected period:

| Indicator | Formula | Interpretation |
|---|---|---|
| **Total Return (%)** | `(P_end / P_start − 1) × 100` | Absolute gain/loss over the period |
| **Annualised Return (%)** | `(P_end / P_start)^(252/n) − 1` | Return normalised to 252 trading days |
| **Annualised Volatility (%)** | `σ(log returns) × √252 × 100` | Risk — dispersion of daily returns |
| **Sharpe Ratio** | `μ(log returns) / σ(log returns) × √252` | Risk-adjusted return (rf = 0) |
| **Max Drawdown (%)** | `min((P − cummax(P)) / cummax(P))` | Worst peak-to-trough decline |
| **Max / Min Price** | `max(High)` / `min(Low)` | Price range over the period |
| **Average Volume** | `mean(Volume)` | Liquidity indicator |

> Sharpe ratio assumes a risk-free rate of **zero**.

---

## 🧪 Statistical Tests

### Normality — `normality_test(x)`

Tests whether the closing-price (or return) distribution is normal.
Useful before applying parametric financial models.

```r
normality_test(df$Close)
```

### Stationarity — `stationarity_test(x, type.test)`

| Test | Null Hypothesis | Min. Obs. |
|---|---|---|
| Box-Pierce / Ljung-Box | No autocorrelation | 3 |
| KPSS | Series is stationary | 3 |
| Augmented Dickey-Fuller (ADF) | Unit root present (non-stationary) | 7 |
| Phillips-Perron | Unit root present (non-stationary) | 4 |

```r
# All tests
stationarity_test(df$Close, type.test = "ALL")

# Single test
stationarity_test(df$Close, type.test = "augmented dickey-fuller test (adf)")
```

- All tests return **p-values** as a named list.
- `type.test` matching is **case-insensitive**.
- Unknown test names are dropped with a diagnostic message.
- p < 0.05 → significant · p ≥ 0.05 → not significant.

---

## 📊 Function Reference

| Function | Key Arguments | Returns |
|---|---|---|
| `GET_tickers(market_code)` | `"BRVM"`, `"NGX"`, …, `"ALL"` | S4 `african_market` or named list |
| `GET_data(market_code, ticker, from, to, output_format)` | See above | `data.frame` or named list |
| `pplot(market, stock, from, to, up.col, down.col)` | `data.frame` or market code + ticker vector | `highchart` object |
| `normality_test(x)` | Numeric vector or `ts` | Named list of p-values |
| `stationarity_test(x, type.test)` | Numeric vector or `ts` | Named list of p-values |
| `RECOVER_last_download()` | — | Last cached `GET_data()` result |

---

## 🧠 Use Cases

- 📐 **Academic research** — market efficiency, volatility clustering, cointegration
- 💼 **Portfolio management** — cross-market diversification, risk budgeting
- 🔬 **Econometrics** — unit root tests, GARCH modelling, event studies
- 📉 **Quantitative finance** — factor models, backtesting strategies
- 🗺️ **Data journalism** — visualising African market trends
- 🎓 **Teaching** — real market data for financial analysis courses

---

## ⚙️ Dependencies

| Category | Packages |
|---|---|
| Data wrangling | `dplyr`, `tidyr`, `tibble`, `purrr`, `magrittr`, `stringr` |
| HTTP & web scraping | `httr`, `httr2`, `rvest`, `jsonlite` |
| Time series & dates | `xts`, `lubridate`, `tseries` |
| Statistics | `nortest`, `fBasics`, `goftest`, `stats` |
| Visualisation | `highcharter` |
| OOP & utilities | `methods`, `rlang` |

---

## ⚠️ Important Notes

- An **active internet connection** is required at runtime.
- **Data depth varies by exchange** — BRVM and GSE are scraped directly;
  NGX, JSE, MSE, EGX, and TSE rely on the Investing.com API.
- **API rate limits** may apply for Investing.com-backed markets.
  If a request fails, wait a few seconds and retry.
- All dates are returned in the **local timezone** of the R session.
- The Sharpe ratio uses a **risk-free rate of zero**.
- The package is in **experimental** lifecycle — the API may change
  before a stable CRAN release.

---

## 👥 Authors

| Name | Role | Contact |
|---|---|---|
| **Olabiyi Aurel Géoffroy Odjo** | Creator & maintainer | odjoaurel@gmail.com |
| **Koffi Frederic Sessie** | Author | koffisessie@gmail.com |
| **Abdoul Oudouss Diakité** | Author | abdouloudoussdiakite@gmail.com |
| **Steven P. Sanderson II, MPH** | Author | spsanderson@gmail.com |

---

## 🤝 Contributing

Contributions are welcome — bug fixes, new markets, documentation improvements.

```bash
# 1. Fork the repo and clone it
git clone https://github.com/Koffi-Fredysessie/AfriMarkets.git

# 2. Create a feature branch
git checkout -b feature/add-xyz-market

# 3. Commit your changes (Conventional Commits style)
git commit -m "feat: add XYZ market support"

# 4. Push and open a Pull Request
git push origin feature/add-xyz-market
```

> Please open an **issue** first to discuss major changes before submitting a PR.

**Adding a new market requires only two steps:**

1. Add an entry to `.MARKET_REGISTRY` in `GET_tickers.R`
2. Add a matching entry to `.DATA_REGISTRY` in `GET_data.R`

No other files need to be modified.

---

## 🐛 Bug Reports & Feature Requests

Found a bug or missing data?  
👉 [Open an issue](https://github.com/Koffi-Fredysessie/AfriMarkets/issues)

Please include:

- R version → `R.version`
- AfriMarkets version → `packageVersion("AfriMarkets")`
- Full error message or unexpected output
- Minimal reproducible example

---

## 📄 License

This project is licensed under the **MIT License** — see the
[LICENSE](LICENSE) file for full details.

---

<div align="center">

Made with ❤️ for African financial markets

⭐ If AfriMarkets is useful to you, please **star the repository** — it helps others discover it.

</div>
