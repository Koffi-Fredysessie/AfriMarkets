#' Retrieve Historical Market Data for African Exchanges
#'
#' @description
#' This generic function fetches historical price and volume data for one or more
#' securities listed on supported African markets (e.g., BRVM, BVC). It standardizes
#' the data retrieval process across different market structures.
#'
#' @param market_code A \code{MARKET} object (e.g., \code{BRVM_MARKET}, \code{BVC_MARKET})
#' representing the specific stock exchange.
#' @param ticker A character vector specifying the security tickers.
#' Tickers are case-insensitive. Special keywords include:
#' \itemize{
#'   \item \code{"ALL"}: Retrieves data for all available securities (shares and indexes).
#'   \item \code{"ALL SHARES"}: Retrieves data for all listed stocks.
#'   \item \code{"ALL INDEXES"}: Retrieves data for all market indexes.
#'   \item A specific vector: e.g., \code{c("ECOC", "SGBCI")}.
#' }
#' @param Period A character string or integer defining the data frequency:
#' \itemize{
#'   \item \code{"daily"} (default) or \code{0}
#'   \item \code{"weekly"} or \code{7}
#'   \item \code{"monthly"} or \code{30}
#'   \item \code{"quarterly"} or \code{91}
#'   \item \code{"yearly"} or \code{365}
#' }
#' @param from Start date of the period (\code{Date} object or \code{"YYYY-MM-DD"}).
#' Defaults to 89 days before the current date.
#' @param to End date of the period (\code{Date} object or \code{"YYYY-MM-DD"}).
#' Defaults to the current date.
#' @param output_format A character string indicating the desired data structure:
#' \code{"by_col"} (long format, default) or \code{"by_row"} (wide format).
#'
#' @details
#' The function automatically formats tickers to uppercase. It handles server-side
#' pagination to ensure seamless retrieval of long-term historical data without
#' connection timeouts.
#'
#' \strong{Output Structures:}
#' \itemize{
#'   \item \strong{\code{"by_col"}}: Returns a "long" data frame with a \code{Ticker}
#'   column. Ideal for filtering and \code{ggplot2} visualizations.
#'   \item \strong{\code{"by_row"}}: Returns a "wide" data frame where each security
#'   has its own set of OHLCV columns (e.g., \code{TICKER.Close}). Best suited for
#'   portfolio optimization and quantitative analysis.
#' }
#'
#' @return A data frame containing: \code{Date}, \code{Open}, \code{High},
#' \code{Low}, \code{Close}, and \code{Volume}.
#'
#' @family Data Retrieval
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
#'
#' @importFrom methods setGeneric setMethod
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch daily data for a specific ticker on BRVM
#' df <- GET_data(market_code = "BRVM", ticker = "SNTS", from = "2023-01-01")
#'
#' # Fetch monthly data for all indexes in wide format
#' indexes_wide <- GET_data(market_code = "BRVM", ticker = "ALL INDEXES",
#'                         Period = "monthly", output_format = "by_row")
#' }
#' @name GET_data
setGeneric("GET_data", function(market_code,ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row")) standardGeneric("GET_data"))


#' @rdname GET_data
#' @export
setMethod(
    "GET_data",
    signature(market_code = "character",ticker = "character"),
    function(market_code, ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row")) {
        tryCatch({

            market_code <- toupper(market_code)

            all_markets <- sapply(CREATE_ALL_MARKETS(),
                                  function(x) x@Market_short_name)

            if (!market_code %in% all_markets) {
                rlang::inform(paste0(
                    "The market '", market_code,
                    "' is not available in this version of AfriMarkets."
                ))
                return(NULL)
            }

            # Dispatcher

            switch(market_code,

                   "BRVM" = .GET_data_BRVM(ticker = ticker,Period = Period,from = from,to = to,output_format = output_format),
                   "BVC"  = NULL,

                   rlang::inform(
                       paste0("No GET_tickers() method defined for ", market_code)
                   )
            )

                },
                error = function(e) {
                    message("Make sure you have an active internet connection")
                },
                warning = function(w) {
                    message("Make sure you have an active internet connection")
                }
            )
})
