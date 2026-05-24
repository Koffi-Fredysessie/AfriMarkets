#' @title Retrieve Historical Market Data for African Stock Exchanges
#'
#' @description
#' Retrieves historical OHLCV (Open, High, Low, Close, Volume) data for one or more
#' financial instruments listed on supported African stock exchanges.
#'
#' This S4 generic provides a unified interface to access heterogeneous data sources
#' (e.g., BRVM scrapers, Investing.com API) while returning a standardized output format.
#'
#' @param market_code A character string specifying the target stock exchange.
#' Supported values include: \code{"BRVM"}, \code{"GSE"}, \code{"NGX"},
#' \code{"JSE"}, \code{"MSE"}, \code{"EGX"}, and \code{"TSE"}.
#'
#' @param ticker A character vector of tickers (case-insensitive).
#' Special values include:
#' \itemize{
#'   \item \code{"ALL"}: all instruments (shares + indices)
#'   \item \code{"ALL SHARES"}: only equities
#'   \item \code{"ALL INDEXES"}: only indices
#' }
#'
#' @param Period A character string indicating frequency.
#' Currently only \code{"daily"} is supported for most markets.
#'
#' @param from Start date (\code{Date} or \code{"YYYY-MM-DD"}).
#' Default is 89 days before today.
#'
#' @param to End date (\code{Date} or \code{"YYYY-MM-DD"}).
#' Default is today.
#'
#' @param output_format Output format:
#' \itemize{
#'   \item \code{"by_col"}: long format (default)
#'   \item \code{"by_row"}: wide format
#'   \item \code{"all"}: returns both formats as a list
#' }
#'
#' @return
#' \itemize{
#'   \item A data frame in long format (\code{by_col})
#'   \item A data frame in wide format (\code{by_row})
#'   \item A list containing both formats (\code{all})
#' }
#'
#' Long format contains:
#' \itemize{
#'   \item \code{Date}, \code{Ticker}, \code{Open}, \code{High},
#'   \item \code{Low}, \code{Close}, \code{Volume}
#' }
#'
#' @details
#' The function acts as a dispatcher:
#' \enumerate{
#'   \item Validates the market code
#'   \item Normalizes tickers
#'   \item Routes the request to a market-specific backend
#'   \item Standardizes output format
#' }
#'
#' Backends:
#' \itemize{
#'   \item BRVM: custom scraping engine
#'   \item Others: Investing.com API via \code{.GET_DATA_FROM_INVESTING()}
#' }
#'
#' @section Limitations:
#' \itemize{
#'   \item Only daily frequency is guaranteed
#'   \item API rate limits may apply
#'   \item Data availability depends on external providers
#' }
#'
#' @family Market Data Functions
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @examples
#' \dontrun{
#' # Single ticker
#' df <- GET_data("NGX", ticker = "ZENITHBANK")
#'
#' # All shares
#' df <- GET_data("GSE", ticker = "ALL SHARES")
#'
#' # Multiple tickers
# df <- GET_data("BRVM", ticker = c("SNTS", "SGBCI"))
#'
#' # Wide format
#' df <- GET_data("NGX", ticker = "ALL", output_format = "by_row")
#' }
#'
#' @name GET_data
#' @export
setGeneric("GET_data", function(market_code,ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row","all")) standardGeneric("GET_data"))


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
                   "GSE"  = .GET_data_GSE(ticker = ticker,Period = Period,from = from,to = to,output_format = output_format),
                   "NGX"  = .GET_DATA_FROM_INVESTING(market_code = "NGX",market_id = "20",ticker = ticker,Period = Period,from = from,to = to,output_format = output_format,base_url = "https://ng.investing.com"),
                   "JSE"  = .GET_DATA_FROM_INVESTING(market_code = "JSE",market_id = "110",ticker = ticker,Period = Period,from = from,to = to,output_format = output_format,base_url = "https://ng.investing.com"),
                   "MSE"  = .GET_DATA_FROM_INVESTING(market_code = "MSE",market_id = "105",ticker = ticker,Period = Period,from = from,to = to,output_format = output_format,base_url = "https://ng.investing.com"),
                   "EGX"  = .GET_DATA_FROM_INVESTING(market_code = "EGX",market_id = "59",ticker = ticker,Period = Period,from = from,to = to,output_format = output_format,base_url = "https://ng.investing.com"),
                   "TSE"  = .GET_DATA_FROM_INVESTING(market_code = "TSE",market_id = "202",ticker = ticker,Period = Period,from = from,to = to,output_format = output_format,base_url = "https://ng.investing.com"),

                   rlang::inform(
                       paste("No GET_data() method defined for", market_code)
                   )
            ))

                },
                error = function(e) {
                    print(e)
                    message("Make sure you have an active internet connection")
                }
            )
})
