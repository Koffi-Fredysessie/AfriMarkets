# On library attachment, print message to user.
.onAttach <- function(libname, pkgname) {
    msg <- paste0(
        "\n",
        "== Welcome to AfriMarkets (xgen version) ================================================================",
        "\nIf you find this package useful, please leave a star: ",
        "\n   https://github.com/Koffi-Fredysessie/AfriMarkets",
        "\n",
        "\nIf you encounter a bug or want to request an enhancement please file an issue at:",
        "\n   https://github.com/Koffi-Fredysessie/AfriMarkets/issues",
        "\n",
        "\nThank you for using AfriMarkets!",
        "\n"
    )

    packageStartupMessage(msg)
}

