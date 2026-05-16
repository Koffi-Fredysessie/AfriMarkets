#' @title Get African Markets Tickers
#'
#' @description
#' Retrieves and processes ticker data (indexes and shares) for supported African
#' financial markets. This function acts as a dispatcher, automatically selecting
#' the appropriate data extraction method based on the provided market code.
#'
#' For each supported market, the function returns a standardized S4 object
#' of class \code{"african_market"} containing market metadata, ticker lists,
#' and detailed information on indexes and shares.
#'
#' @param market_code A character string indicating the African market code
#'   (e.g. \code{"BRVM"}, \code{"NGX"}, \code{"JSE"}). Use \code{"ALL"} to
#'   retrieve all supported markets at once.
#'
#' @return
#' \itemize{
#'   \item If a single market is requested: an S4 object of class \code{"african_market"}.
#'   \item If \code{"ALL"} is used: a named list of S4 market objects.
#' }
#'
#' Each market object contains:
#' \itemize{
#'   \item \code{Indexes}: Data frame of index tickers
#'   \item \code{Shares}: Data frame of share tickers
#'   \item \code{ListIndexes}: Character vector of index tickers
#'   \item \code{ListShares}: Character vector of share tickers
#'   \item \code{List}: Combined vector of all tickers
#'   \item \code{Ticker_full_name}: Alias of \code{List}
#' }
#'
#' @details
#' The function supports multiple African stock exchanges using a unified interface.
#' Internally, it dispatches calls to market-specific implementations:
#'
#' \itemize{
#'   \item \code{BRVM}: Custom scraper
#'   \item Other markets (NGX, JSE, EGX, etc.): Investing.com API
#' }
#'
#' Supported markets include:
#' \itemize{
#'   \item BRVM – Bourse Régionale des Valeurs Mobilières (UEMOA)
#'   \item GSE – Ghana Stock Exchange
#'   \item NGX – Nigerian Exchange
#'   \item JSE – Johannesburg Stock Exchange
#'   \item MSE – Casablanca Stock Exchange
#'   \item EGX – Egyptian Exchange
#'   \item TSE – Tunis Stock Exchange
#' }
#'
#' Additional markets can be added without modifying the user-facing API.
#'
#' If an unsupported market is requested, an informative message is returned.
#'
#' @section Performance:
#' Data retrieval depends on external APIs and may vary in speed depending on
#' network conditions and server response times.
#'
#' @author
#' Olabiyi Aurel Geoffroy ODJO \cr
#' Koffi Frederic SESSIE \cr
#' Oudouss Diakite Abdoul \cr
#' Steven P. Sanderson II, MPH
#'
#' @examples
#' \dontrun{
#' # Retrieve BRVM tickers
#' brvm <- GET_tickers("BRVM")
#'
#' # Retrieve NGX tickers
#' ngx <- GET_tickers("NGX")
#'
#' # Retrieve all markets
#' markets <- GET_tickers("ALL")
#'
#' # Access shares
#' ngx@Shares
#'
#' # List all tickers
#' ngx@List
#' }
#'
#' @rdname GET_tickers
#' @export
setGeneric("GET_tickers", function(market_code) standardGeneric("GET_tickers" ))


#' @rdname GET_tickers
#' @export
setMethod("GET_tickers", signature(market_code = "character"),
          function(market_code) {

              output = NULL
              market_code <- toupper(market_code)
              ALL_MARKETS = CREATE_ALL_MARKETS()

              all_markets <- sapply(ALL_MARKETS,
                                    function(x) x@Market_short_name)

              if(market_code == "ALL"){
                  output = list(
                     "BRVM" = .GET_tickers_BRVM(),
                     "GSE"  = .GET_tickers_GSE(), #.GET_TICKER_FROM_INVESTING(ALL_MARKETS$GSE_MARKET,market_id = "3"), #Ghana
                     "NGX"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$NGX_MARKET,market_id = "20"), #Nigeria
                     "JSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$JSE_MARKET,market_id = "110"), #South Africa
                     "MSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$MSE_MARKET,market_id = "105"), #Morocco
                     "EGX"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$EGX_MARKET,market_id = "59"), #Egypt
                     "TSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$TUNIS_MARKET,market_id = "202") #Tunis
                     #"NSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$NSE_KENYA_MARKET,market_id = "57"), #Kenya
                     #"BSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$BSE_MARKET,market_id = "163"), #Botswana
                     #"SEM"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$SEM_MARKET,market_id = "188") #Mauritius
                  )
              } else {

                  if (!market_code %in% all_markets) {
                      rlang::inform(paste0(
                          "The market '", market_code,
                          "' is not available in this version of AfriMarkets."
                      ))
                      return(NULL)
                  }

                  # Dispatcher
                  output = switch(market_code,
                         "BRVM" = .GET_tickers_BRVM(),
                         "GSE"  = .GET_tickers_GSE(), #.GET_TICKER_FROM_INVESTING(ALL_MARKETS$GSE_MARKET,market_id = "3"), #Ghana
                         "NGX"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$NGX_MARKET,market_id = "20"), #Nigeria
                         "JSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$JSE_MARKET,market_id = "110"), #South Africa
                         "MSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$MSE_MARKET,market_id = "105"), #Morocco
                         "EGX"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$EGX_MARKET,market_id = "59"), #Egypt
                         "TSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$TUNIS_MARKET,market_id = "202"), #Tunis
                         #"NSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$NSE_KENYA_MARKET,market_id = "57"), #Kenya
                         #"BSE"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$BSE_MARKET,market_id = "163"), #Botswana
                         #"SEM"  = .GET_TICKER_FROM_INVESTING(ALL_MARKETS$SEM_MARKET,market_id = "188"), #Mauritius
                         rlang::inform(
                             paste0("No GET_tickers() method defined for ", market_code)
                         )
                  )
              }

              .pkg_env$last_ticker_downloaded = output

              return(output)

          })


