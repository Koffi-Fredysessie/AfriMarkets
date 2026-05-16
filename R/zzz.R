.onAttach <- function(libname, pkgname) {

    markets <- paste(
        lapply(CREATE_ALL_MARKETS(),
               function(x) paste0(" - ", x@Market_full_name, " (", x@Market_short_name, ")")),
        collapse = "\n"
    )

    msg <- paste0(
        "\n",
        "============================================================\n",
        " \U0001F680 AfriMarkets [XGEN EDITION]\n",
        "============================================================\n",
        " \U0001F4CA African Financial Markets Data Package\n\n",
        "------------------------------------------------------------\n",
        "\n",
        " \U0001F30D AVAILABLES MARKETS\n\n",
        "------------------------------------------------------------\n",
        markets,
        "\n",
        "------------------------------------------------------------\n",
        "\n",
        " \U0001F464 Authors:\n",
        "   \u2022 Olabiyi Aurel G\u00E9offroy ODJO\n",
        "   \u2022 Koffi Fr\u00E9d\u00E9ric SESSIE\n",
        "\n",
        " \U0001F517 Project links:\n",
        "   \u2022 https://github.com/Koffi-Fredysessie/AfriMarkets\n",
        "\n",
        " \U0001F41B Issues / bug reports:\n",
        "   \u2022 https://github.com/Koffi-Fredysessie/AfriMarkets/issues\n",
        "\n",
        " \U0001F64F Thank you for using AfriMarkets!\n\n",
        "   Empowering African financial data \U0001F30D\U0001F4C8\n",
        "\n",
        "============================================================\n"
    )

    packageStartupMessage(msg)
}
