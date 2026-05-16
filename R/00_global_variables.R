utils::globalVariables(
    names = c(

        # =====================================================
        # 🔹 ENVIRONNEMENT & OBJETS GLOBAUX
        # =====================================================
        ".pkg_env",
        "all_markets",
        "index_url",
        "field_transact_time",
        "created",
        "nonce_id",
        "full_join",

        # =====================================================
        # 🔹 COLONNES STANDARD (DATA FRAMES)
        # =====================================================
        "Date", "Ticker", "Open", "High", "Low", "Close", "Volume",

        # =====================================================
        # 🔹 INFORMATIONS ENTREPRISE / MARCHÉ
        # =====================================================
        "CompanyName",
        "NatureofBusiness",
        "Sector",
        "Symbol",
        "Type",
        "Country.code",
        "Direction",

        # =====================================================
        # 🔹 VARIABLES INTERNES / TRANSFORMATION
        # =====================================================
        "label", "rename", "value",
        "ticker_html",
        ".stationarity_core",

        # =====================================================
        # 🔹 NSE / DPLYR HELPERS
        # =====================================================
        ".",

        # =====================================================
        # 🔹 FONCTIONS INTERNES
        # =====================================================
        ".GET_data_BVC",
        ".GET_data_BRVM",
        ".GET_data_GSE",
        "get_bvc_index",
        "get_index_code",
        "gse_share_info",
        ".get_class_marche",
        "get_gse_wdt",

        # =====================================================
        # 🔹 DONNÉES INVESTING (RAW API)
        # =====================================================
        "rowDateTimestamp",
        "last_close",
        "last_open",
        "last_max",
        "last_min",
        "volumeRaw",

        # =====================================================
        # 🔹 IDENTIFIANTS & MÉTADONNÉES
        # =====================================================
        "symbol", "SYMBOL",
        "id", "Id",
        "name", "Name",
        "flag", "Flag",
        "URL", "Url",
        "link",
        "key",

        # =====================================================
        # 🔹 FONDAMENTAUX
        # =====================================================
        "FundamentalMarketCap",
        "FundamentalBeta"
    )
)
