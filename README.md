# 📦 AfriMarkets

**AfriMarkets** is an R package designed to retrieve and analyze African stock market data from multiple exchanges, including BRVM, BVC, GSE, and NGX.

It provides simple functions to:
- Fetch available tickers per market
- Download historical stock data
- Standardize financial datasets across exchanges

---

## 🚀 Features

- 📊 Retrieve tickers from multiple African stock exchanges
- 📈 Download historical stock price data
- 🔄 Support for multiple data formats (long and wide)
- ⚡ Simple and consistent API
- 🧹 Clean and ready-to-analyze outputs

---

## 📦 Installation

```r
# Install from GitHub (if available)
devtools::install_github("your-username/AfriMarkets")
```

---

## 🔑 Supported Markets

- 🇧🇫 BRVM (Bourse Régionale des Valeurs Mobilières)
- 🇧🇯 BVC (Bourse des Valeurs du Bénin)
- 🇬🇭 GSE (Ghana Stock Exchange)
- 🇳🇬 NGX (Nigerian Exchange Group)

---

## 📌 Usage

### Load package
```r
library(AfriMarkets)
```

### Retrieve tickers
```r
GET_tickers("brvm")
GET_tickers("bvc")
GET_tickers("gse")
GET_tickers("ngx")
```

### Retrieve BRVM data
```r
df1 <- GET_data("BRVM", ticker = "SNTS", from = "2023-01-01")
```

### Retrieve GSE data (long format)
```r
df2 <- GET_data("GSE", ticker = "ACCESS", Period = "daily")
```

### Retrieve BVC data (wide format)
```r
df3 <- GET_data("BVC", ticker = c("ADH", "Afm"), output_format = "by_row")
```

---

## 🔄 Output Formats

| Format | Description |
|--------|-------------|
| long | Tidy time-series format |
| by_row | Wide format grouped by ticker |

---

## 📊 Function Overview

### GET_tickers(exchange)
Returns available tickers for a given exchange.

### GET_data(exchange, ticker, ...)
Downloads historical stock data.

---

## 🧠 Use Cases

- Financial research
- Time-series modeling
- Portfolio analysis
- Academic research
- Data visualization

---

## ⚠️ Notes

- Internet connection required
- Data availability varies by exchange

---

## 👨‍💻 Author
AfriMarkets development team
