#' Retrieve Historical Market Data for African Stock Exchanges
#'
#' @description
#' This generic S4 method retrieves historical price and volume data for one or more
#' securities listed on supported African stock exchanges (e.g., BRVM, BVC, GSE).
#' It provides a unified interface for accessing heterogeneous market data sources
#' through a consistent output structure.
#'
#' @param market_code A character string specifying the target stock exchange.
#' Supported values include: \code{"BRVM"}, \code{"BVC"}, and \code{"GSE"}.
#'
#' @param ticker A character vector of security tickers. Tick symbols are case-insensitive.
#' Special keywords include:
#' \itemize{
#'   \item \code{"ALL"}: retrieves all available securities (shares and indices)
#'   \item \code{"ALL SHARES"}: retrieves only listed equities
#'   \item \code{"ALL INDEXES"}: retrieves only market indices
#'   \item a custom vector such as \code{c("ACCESS", "SGBCI")}
#' }
#'
#' @param Period A character string or numeric value defining data frequency:
#' \itemize{
#'   \item \code{"daily"} or \code{0} (default)
#'   \item \code{"weekly"} or \code{7}
#'   \item \code{"monthly"} or \code{30}
#'   \item \code{"quarterly"} or \code{91}
#'   \item \code{"yearly"} or \code{365}
#' }
#'
#' @param from Start date of the time window (\code{Date} or \code{"YYYY-MM-DD"}).
#' Default is 89 days before \code{Sys.Date()}.
#'
#' @param to End date of the time window (\code{Date} or \code{"YYYY-MM-DD"}).
#' Default is the current date.
#'
#' @param output_format Output format specification:
#' \itemize{
#'   \item \code{"by_col"} (default): long format with a \code{Ticker} column, suitable for analysis and visualization
#'   \item \code{"by_row"}: wide format where each ticker is expanded into separate columns
#' }
#'
#' @details
#' The function acts as a dispatcher that routes requests to the appropriate
#' market-specific data retrieval function based on \code{market_code}.
#' It ensures consistent preprocessing, ticker normalization, and output formatting
#' across all supported exchanges.
#'
#' Internally, the function handles:
#' \itemize{
#'   \item Market validation using available registered markets
#'   \item Ticker normalization (uppercase conversion)
#'   \item Error handling for unsupported markets or failed requests
#'   \item Delegation to market-specific implementations
#' }
#'
#' @return A data frame containing standardized market data including:
#' Date, Open, High, Low, Close, Volume, and additional market-specific fields.
#'
#' @family Market Data Functions
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
#'
#' @importFrom methods setGeneric setMethod
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve BRVM daily stock data
#' df <- GET_data("BRVM", ticker = "SNTS", from = "2023-01-01")
#'
#' # Retrieve all GSE shares in long format
#' df <- GET_data("GSE", ticker = "ALL SHARES", Period = "daily")
#'
#' # Retrieve BVC data in wide format
#' df <- GET_data("BVC", ticker = c("ACCESS", "CAL"), output_format = "by_row")
#' }
#'
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

            return(switch(market_code,

                   "BRVM" = .GET_data_BRVM(ticker = ticker,Period = Period,from = from,to = to,output_format = output_format),
                   "BVC"  = .GET_data_BVC(ticker = ticker,Period = Period,from = from,to = to,output_format = output_format),
                   "GSE"  = .GET_data_GSE(ticker = ticker,Period = Period,from = from,to = to,output_format = output_format),

                   rlang::inform(
                       paste0("No GET_tickers() method defined for ", market_code)
                   )
            ))

                },
                error = function(e) {
                    message("Make sure you have an active internet connection")
                }
            )
})
