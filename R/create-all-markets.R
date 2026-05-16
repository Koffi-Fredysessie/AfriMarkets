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
            # =========================
            # AFRICAN MARKETS
            # =========================

            list(

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

                # Nigeria (20)
                NGX_MARKET = african_market(
                    Market_short_name = "NGX",
                    Market_full_name = "NIGERIAN EXCHANGE GROUP",
                    Official_url = "https://www.ngxgroup.com",
                    Market_url = "/equities/nigeria",
                    Market_data_url = c("/equities/nigeria"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # South Africa (110)
                JSE_MARKET = african_market(
                    Market_short_name = "JSE",
                    Market_full_name = "JOHANNESBURG STOCK EXCHANGE",
                    Official_url = "https://www.jse.co.za",
                    Market_url = "/equities/south-africa",
                    Market_data_url = c("/equities/south-africa"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # Morocco (105)
                MSE_MARKET = african_market(
                    Market_short_name = "MSE",
                    Market_full_name = "MOROCCO STOCK EXCHANGE",
                    Official_url = "https://www.casablancabourse.com",
                    Market_url = "/equities/morocco",
                    Market_data_url = c("/equities/morocco"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # Egypt (59)
                EGX_MARKET = african_market(
                    Market_short_name = "EGX",
                    Market_full_name = "EGYPTIAN EXCHANGE",
                    Official_url = "https://www.egx.com.eg",
                    Market_url = "/equities/egypt",
                    Market_data_url = c("/equities/egypt"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # Kenya (57)
                NSE_KENYA_MARKET = african_market(
                    Market_short_name = "NSE",
                    Market_full_name = "NAIROBI SECURITIES EXCHANGE",
                    Official_url = "https://www.nse.co.ke",
                    Market_url = "/equities/kenya",
                    Market_data_url = c("/equities/kenya"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # Côte d'Ivoire (78)
                BRVM_MARKET = african_market(
                    Market_short_name = "BRVM",
                    Market_full_name = "BOURSE REGIONALE DES VALEURS MOBILIERES",
                    Official_url = "https://www.brvm.org",
                    Market_url = "/equities/ivory-coast",
                    Market_data_url = c("https://www.sikafinance.com/api/general/GetHistos","/equities/ivory-coast"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),


                # Tunisia (202)
                TUNIS_MARKET = african_market(
                    Market_short_name = "TSE",
                    Market_full_name = "TUNIS STOCK EXCHANGE",
                    Official_url = "https://www.bvmt.com.tn",
                    Market_url = "/equities/tunisia",
                    Market_data_url = c("/equities/tunisia"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # Botswana (163)
                BSE_MARKET = african_market(
                    Market_short_name = "BSE",
                    Market_full_name = "BOTSWANA STOCK EXCHANGE",
                    Official_url = "https://www.bse.co.bw",
                    Market_url = "/equities/botswana",
                    Market_data_url = c("/equities/botswana"),
                    List = "",
                    ListShares = "",
                    ListIndexes = "",
                    ListBonds = "",
                    Indexes = data.frame(),
                    Shares = data.frame(),
                    Bonds = data.frame(),
                    Ticker_full_name = ""
                ),

                # Mauritius (188)
                SEM_MARKET = african_market(
                    Market_short_name = "SEM",
                    Market_full_name = "STOCK EXCHANGE OF MAURITIUS",
                    Official_url = "https://www.stockexchangeofmauritius.com",
                    Market_url = "/equities/mauritius",
                    Market_data_url = c("/equities/mauritius"),
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
