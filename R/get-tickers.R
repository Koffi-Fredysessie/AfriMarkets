#' @title Get African Markets Tickers
#'
#' @description
#' Retrieves and processes ticker data (indexes and shares) for supported African financial markets.
#' The function automatically dispatches the appropriate data extraction logic based on the
#' provided market code (e.g. \code{"BRVM"}, \code{"BVC"}).
#'
#' For each supported market, the function returns a standardized S4 object containing
#' market metadata, lists of tickers, detailed index and share information, and related
#' descriptive fields.
#'
#' @param market_code A character string indicating the African market code for which
#' ticker data should be retrieved (e.g. \code{"BRVM"}, \code{"BVC"}).
#'
#' @return
#' An S4 object of class \code{"african_market"} populated with ticker data, including:
#' \itemize{
#'   \item market metadata (name, URLs),
#'   \item lists of index and share tickers,
#'   \item detailed data frames for indexes and shares.
#' }
#'
#' @details
#' This function provides a unified interface for accessing ticker information across
#' multiple African stock exchanges. Each market is processed using its own dedicated
#' data extraction logic while preserving a common output structure.
#'
#' Supported markets may include, but are not limited to:
#' \itemize{
#'   \item BRVM – Bourse Régionale des Valeurs Mobilières (WAEMU / UEMOA),
#'   \item BVC – Bourse des Valeurs du Cameroun.
#'   \item GSE – Ghana Stock Exchange.
#' }
#'
#' Additional markets can be integrated seamlessly in future releases without changing
#' the user-facing API.
#'
#' @importFrom methods setGeneric setMethod
#' @import rvest
#' @import httr2
#' @import tidyr
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO \cr
#' Oudouss Diakite Abdoul \cr
#' Steven P. Sanderson II, MPH
#'
#' @examples
#' \dontrun{
#' # Retrieve BRVM tickers
#' brvm_ticks <- GET_tickers("BRVM")
#'
#' # Retrieve BVC tickers
#' bvc_ticks <- GET_tickers("BVC")
#'
#' # Display shares
#' brvm_ticks@Shares
#'
#' # List of all tickers
#' brvm_ticks@List
#'
#' # Print market object
#' brvm_ticks
#' }
#'
#' @rdname GET_tickers
#' @export
setGeneric("GET_tickers", function(market_code) standardGeneric("GET_tickers" ))


#' @rdname GET_tickers
#' @export
setMethod("GET_tickers", signature(market_code = "character"),
          function(market_code) {

              market_code <- toupper(market_code)

              all_markets <- sapply(CREATE_ALL_MARKETS(),
                                    function(x) x@Market_short_name)

              if(market_code == "ALL"){
                  list(
                     "BRVM" = .GET_tickers_BRVM(),
                     "BVC"  = .GET_tickers_BVC(),
                     "GSE"  = .GET_tickers_GSE(),
                     "NGX"  = .GET_tickers_NGX()
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
                  switch(market_code,
                         "BRVM" = .GET_tickers_BRVM(),
                         "BVC"  = .GET_tickers_BVC(),
                         "GSE"  = .GET_tickers_GSE(),
                         "NGX"  = .GET_tickers_NGX(),
                         rlang::inform(
                             paste0("No GET_tickers() method defined for ", market_code)
                         )
                  )
              }

          })


