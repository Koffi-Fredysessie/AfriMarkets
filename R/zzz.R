.onAttach <- function(libname, pkgname) {

    markets <- paste(
        lapply(CREATE_ALL_MARKETS(),
               function(x) paste0(" - ", x@Market_full_name, " (", x@Market_short_name, ")")),
        collapse = "\n"
    )

    msg <- paste0(
        "\n",
        "============================================================\n",
        " AfriMarkets [XGEN VERSION]\n",
        "============================================================\n",
        "\n",
        "African Financial Markets Data Package\n",
        "\n",
        "Authors:\n",
        " - Koffi Frederic SESSIE\n",
        " - Olabiyi Aurel Geoffroy ODJO\n",
        "\n",
        "Project links:\n",
        " - https://github.com/Koffi-Fredysessie/AfriMarkets\n",
        "\n",
        "Issues / bug reports:\n",
        " - https://github.com/Koffi-Fredysessie/AfriMarkets/issues\n",
        "\n",
        "Thank you for using AfriMarkets.\n",
        "\n",
        "------------------------------------------------------------\n",
        "AVAILABLES MARKETS\n",
        "------------------------------------------------------------\n",
        markets,
        "\n",
        "------------------------------------------------------------\n"
    )

    packageStartupMessage(msg)
}
