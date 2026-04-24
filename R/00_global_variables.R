globalVariables(
    names = c("all_markets","Date", "Ticker", "Open", "High", "Low", "Close", "Volume","index_url","field_transact_time","created",
              "CompanyName",
              "NatureofBusiness",
              ".stationarity_core",
              "Sector", "Symbol", "Type", "label", "rename", "value",
              "Country.code",
              "ticker_html",
              "get_gse_wdt","nonce_id",
              # ---- column names (data frames) ----
              "Date",
              "Ticker",
              "Open",
              "High",
              "Low",
              "Close",
              "Volume",

              # ---- NSE / dplyr helpers ----
              ".",

              # ---- functions flagged by R CMD check ----
              ".GET_data_BVC",
              ".GET_data_BRVM",
              ".GET_data_GSE",
              "get_bvc_index",
              "get_index_code",
              "gse_share_info"

              )
    )
