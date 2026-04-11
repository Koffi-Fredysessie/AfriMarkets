globalVariables(
    names = c("all_markets","Date", "Ticker", "Open", "High", "Low", "Close", "Volume","index_url","field_transact_time",
              "ticker_html",
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
