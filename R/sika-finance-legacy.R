#' Retrieve Historical Market Data from Sika Finance API
#'
#' @description
#' Internal helper function used to retrieve historical OHLCV
#' (Open, High, Low, Close, Volume) market data for one or multiple
#' financial instruments from the Sika Finance API.
#'
#' The function supports multiple aggregation frequencies, flexible
#' date ranges, and several output structures for downstream analysis
#' and visualization workflows.
#'
#' @param ticker A character vector containing one or multiple ticker symbols
#'   to retrieve (case-insensitive).
#'
#' @param market_tickers An S4 market object returned by
#'   \code{\link{GET_tickers}} containing the list of available
#'   instruments and market metadata.
#'
#' @param Period Data frequency specification. Supported values are:
#'   \itemize{
#'     \item \code{"daily"} or \code{0} (default)
#'     \item \code{"weekly"} or \code{7}
#'     \item \code{"monthly"} or \code{30}
#'     \item \code{"quarterly"} or \code{91}
#'     \item \code{"yearly"} or \code{365}
#'   }
#'
#' @param from Start date of the historical extraction window.
#'   Can be either a \code{Date} object or a character string
#'   formatted as \code{"YYYY-MM-DD"}.
#'   Default is 89 days before the current date.
#'
#' @param to End date of the historical extraction window.
#'   Can be either a \code{Date} object or a character string
#'   formatted as \code{"YYYY-MM-DD"}.
#'   Default is the current date.
#'
#' @param output_format Output structure returned by the function:
#'   \itemize{
#'     \item \code{"by_col"}: long/tabular format with a \code{Ticker} column
#'     \item \code{"by_row"}: wide format with ticker-prefixed columns
#'     \item \code{"all"}: returns both formats as a named list
#'   }
#'
#' @details
#' The function internally performs the following operations:
#'
#' \enumerate{
#'   \item Parses and validates the input dates.
#'   \item Converts textual frequencies into API-compatible numeric codes.
#'   \item Matches user-provided tickers against available market instruments.
#'   \item Splits large requests into chunks of approximately 89 days
#'   to comply with API constraints.
#'   \item Sends POST requests to the Sika Finance historical endpoint.
#'   \item Cleans and merges retrieved historical datasets.
#'   \item Returns the final data using the selected output structure.
#' }
#'
#' To reduce server load and limit throttling risks, the function inserts
#' a small random delay between consecutive requests.
#'
#' @return
#' Depending on \code{output_format}, the function returns:
#'
#' \itemize{
#'
#'   \item A data frame in long format (\code{"by_col"}) containing:
#'   \code{Date}, \code{Ticker}, OHLCV variables, and additional
#'   fields returned by the API.
#'
#'   \item A data frame in wide format (\code{"by_row"}) where
#'   variables are prefixed by ticker names
#'   (e.g. \code{BOAB.Close}, \code{BOAB.Volume}).
#'
#'   \item A named list containing both formats when
#'   \code{output_format = "all"}.
#' }
#'
#' @section Frequency Conversion:
#' Character frequency values are automatically mapped to the
#' numeric codes expected by the Sika Finance API:
#'
#' \itemize{
#'   \item \code{daily -> 0}
#'   \item \code{weekly -> 7}
#'   \item \code{monthly -> 30}
#'   \item \code{quarterly -> 91}
#'   \item \code{yearly -> 365}
#' }
#'
#' @section Error Handling:
#' The function stops execution when:
#'
#' \itemize{
#'   \item the start date is greater than or equal to the end date,
#'   \item the start date is too recent,
#'   \item no valid ticker can be matched,
#'   \item an unsupported frequency is supplied.
#' }
#'
#' Informative progress and availability messages are displayed during
#' the extraction process.
#'
#' @section Caching:
#' The most recent successful extraction is cached inside
#' \code{.pkg_env$last_data_downloaded} for reuse during
#' the current R session.
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @seealso
#' \code{\link{GET_data}},
#' \code{\link{GET_tickers}}
#'
#' @importFrom httr2 request req_method req_headers
#' @importFrom httr2 req_body_json req_perform resp_body_json
#' @importFrom dplyr mutate distinct arrange rename_with
#' @importFrom lubridate parse_date_time
#' @importFrom rlang abort
#' @importFrom stats runif
#'
#' @keywords internal
.GET_DATA_FROM_SIKAFINANCE = function(ticker,market_tickers,Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row","all")) {
    # tick = "BOAB"
    first_date <- lubridate::parse_date_time(from, orders = "ymd")
    end_date   <- lubridate::parse_date_time(to, orders = "ymd")

    #Period = "daily"
    if(Period %in% c('daily', 'weekly', 'monthly', 'quarterly','yearly')){
        Period = switch(
            tolower(Period),
            daily = 0,
            weekly = 7,
            monthly = 30,
            quarterly = 91,
            yearly = 365
        )
    }

    if (first_date >= end_date){
        rlang::abort(
            "The '.from' parameter (start_date) must be less than '.to' (end_date)"
        )
    } else if (first_date >= Sys.Date()-2){
        rlang::abort(
            "The '.from' parameter (start_date) must be less than today's date"
        )
    }

    ticker <- unique(toupper(ticker))

    tick_vec <- NULL
    full_ticker_name = market_tickers@Ticker_full_name
    ## Filter ticker in .indexes or all_ticker list

    for (tick in ticker) {
        locate_ticker = startsWith(full_ticker_name,tick)
        if(any(locate_ticker)){
            tick_vec = c(tick_vec,full_ticker_name[which(locate_ticker)])
        }
    }

    # Check input parameters after filtering ----
    if (length(tick_vec) < 1){
        rlang::abort(
            "The 'ticker' parameter cannot be blank. Please enter at least one ticker.
            If entering multiple please use .symbol = c(Tick_1, Tick_2, ...)"
        )
    } else {
        ticker <- tick_vec
    }

    ticker_data1 = NULL
    ticker_data2 = NULL
    nb_merging = 0 # nombre de fusion fait

    if (!(as.numeric(Period) %in% c(0, 7, 30, 91, 365))) {
        rlang::abort("Period not found !")
    }

    for (Tick in ticker) {

        TickName = sub("\\..*", "", Tick)
        asset_data <- NULL
        range_period = seq(from = first_date, to = end_date, by = "89 day")
        ifelse(!(end_date %in% range_period),
               range_period <- c(range_period,end_date),range_period)

        range_period_length = length(range_period)
        range_period_length_adjusted = range_period_length - 1 # parcourir jusqu'a l'avant derniere date

        for(i in 1:range_period_length_adjusted) {  # parcourir les intervalles de periode

            # i = 1
            from_date <- as.Date.POSIXct(range_period[i])
            to_date <- as.Date.POSIXct(range_period[i+1])

            base_request <- request("https://www.sikafinance.com/api/general/GetHistos") %>%
                req_method("POST") %>%
                req_headers(
                    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:144.0) Gecko/20100101 Firefox/144.0",
                    "Accept" = "*/*",
                    "Accept-Language" = "fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3",
                    "Origin" = "https://www.sikafinance.com",
                    "Connection" = "keep-alive",
                    "Referer" = paste0("https://www.sikafinance.com/marches/historiques/", Tick),
                    "Sec-Fetch-Dest" = "empty",
                    "Sec-Fetch-Mode" = "cors",
                    "Sec-Fetch-Site" = "same-origin"
                )

            Sys.sleep(runif(1,0,0.5))

            params = list('ticker'= Tick,
                          'datedeb'= from_date,
                          'datefin'= to_date,
                          'xperiod'= paste0(Period,''))

            period_data <- base_request %>%
                req_body_json(params) %>%
                req_perform() %>%
                resp_body_json(simplifyVector = TRUE)

            if (!is.data.frame(period_data$lst)) {
                next
            }

            period_data <- as.data.frame(period_data$lst)
            asset_data <- rbind(asset_data, period_data)

        }

        if(is.null(asset_data)) {

            message(paste0("[e]\u274C ",TickName," data aren't available between ",
                           first_date,
                           " and ",
                           end_date))
            next
        } else {
            asset_data <- asset_data %>%
                mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
                distinct()

            message(paste0("[100%]\U2705 We obtained ",TickName,  " data from ",
                           min(asset_data$Date),
                           " to ",
                           max(asset_data$Date)))
        }

        # ES
        if (nb_merging == 0){
            ticker_data1 = asset_data %>% mutate(Ticker = TickName) # initialisation
            ticker_data2 <- asset_data # initialisation
        } else if(nb_merging > 0){
            ticker_data1 = rbind(ticker_data1, asset_data %>% mutate(Ticker = TickName))
            ticker_data2 <- merge(ticker_data2, asset_data %>%
                                      rename_with(
                                          ~ paste0(TickName,".",.x),
                                          .cols = -Date
                                      ), by = "Date", all = TRUE)
        }

        nb_merging = nb_merging + 1

    }

    ticker_data1 = ticker_data1 %>% arrange(Date)
    ticker_data2 = ticker_data2 %>% arrange(Date)

    if(output_format[1] == "by_col"){ # agencer suivant les colonnes
        ticker_data = ticker_data1
    } else if(output_format[1] == "by_row"){ # agencer suivant les lignes
        ticker_data = ticker_data2
    } else if(output_format[1] == "all"){ # agencer suivant les lignes
        ticker_data = list(
            by_row = ticker_data1,
            by_col = ticker_data2
        )
    }

    if(.pkg_env$last_cache_operation_is_available == FALSE) {
        .pkg_env$last_data_downloaded = list(
            by_col = ticker_data1,
            by_row = ticker_data2,
            last_returned = ticker_data
        )
        .pkg_env$last_cache_operation_is_available = TRUE

    }

    return(ticker_data) # final output

}

