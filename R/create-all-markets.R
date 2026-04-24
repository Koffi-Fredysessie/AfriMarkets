#' Create and return all available markets
#'
#' @param object ignored (use the "missing" method)
#' @return A list of objects of type \code{market_data} or derived classes
#'
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
#'
#' @importFrom methods setGeneric setMethod
#'
#' @rdname CREATE_ALL_MARKETS
#' @export
setGeneric("CREATE_ALL_MARKETS", function(object) standardGeneric("CREATE_ALL_MARKETS"))


#' @rdname CREATE_ALL_MARKETS
#' @export
setMethod("CREATE_ALL_MARKETS", signature(object = "missing"), function(object) {
    tryCatch(
        {
            # BRVM (1)

            list(
                BRVM_MARKET = african_market(
                    Market_short_name = "BRVM",
                    Market_full_name = "BOURSE REGIONALE DES VALEURS MOBILIERES",
                    Official_url = "https://www.brvm.org/",
                    Market_url = "https://www.sikafinance.com/",
                    Market_data_url = c(
                        "https://www.sikafinance.com/api/general/GetHistos"
                    ),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # BVC (2)

                BVC_MARKET = african_market(
                    Market_short_name = "BVC",
                    Market_full_name = "BOURSE DE CASABLANCA / CASABLANCA STOCK EXCHANGE",
                    Official_url = "https://www.casablanca-bourse.com",
                    Market_url = "",
                    Market_data_url = c(
                        ""
                    ),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # GSE (3)

                GSE_MARKET = african_market(
                    Market_short_name = "GSE",
                    Market_full_name = "GHANA STOCK EXCHANGE",
                    Official_url = "https://www.gsewebportal.com",
                    Market_url = "",
                    Market_data_url = c(
                        ""
                    ),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # NGX (4)

                NGX_MARKET = african_market(
                    Market_short_name = "NGX",
                    Market_full_name = "NIGERIAN EXCHANGE GROUP",
                    Official_url = "https://www.ngxgroup.com",
                    Market_url = "",
                    Market_data_url = c(
                        ""
                    ),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                )


            )
        },
        error = function(e) {
            message(e)
        },
        warning = function(w) {
            message(w)
        }
    )
})
