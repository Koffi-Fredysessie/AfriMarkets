#' Update Market Object with GSE Tickers
#'
#' @description
#' This internal function retrieves ticker data from the Ghana Stock Exchange (GSE)
#' using \code{get_gse_index_share()}, and organizes the results into a structured
#' \code{MARKET} S4 object.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Fetches raw ticker data from the GSE.
#'   \item Optionally sorts the data (if applicable).
#'   \item Separates assets into Shares and Indexes.
#'   \item Updates the slots of the \code{MARKET} S4 object accordingly.
#' }
#'
#' @return An object of class \code{MARKET} containing:
#' \itemize{
#'   \item \code{Shares}: data frame of listed equities
#'   \item \code{Indexes}: data frame of indices (currently not implemented)
#'   \item \code{ListShares}: vector of share tickers
#'   \item \code{List}: combined vector of all available tickers
#'   \item \code{Ticker_full_name}: vector of full ticker names
#' }
#'
#' @note
#' This function is intended for internal use only. It uses \code{tryCatch}
#' to handle potential connection or parsing errors.
#'
#' @importFrom methods new
#'
#' @keywords internal
.GET_tickers_GSE = function() {
    tryCatch(
        {
            # General extraction

            ticker_data = get_gse_index_share()
            ticker_data = ticker_data[order(ticker_data$Country.Code ),]

            # MISE EN FORMAT SELON L'AFFICHAGE STANDARD
            gse_market = CREATE_ALL_MARKETS()$GSE_MARKET

            # Indexes
            gse_market@Indexes = ticker_data[ticker_data$Type == "Index",1:2]
            rownames(gse_market@Indexes) = NULL

            # Shares
            gse_market@Shares = ticker_data[ticker_data$Type == "Share",1:3]
            rownames(gse_market@Shares) = NULL

            # Name List
            gse_market@ListIndexes = gse_market@Indexes[,1]
            gse_market@ListShares = gse_market@Shares[,1]
            gse_market@List = c(gse_market@ListIndexes,gse_market@ListShares) #Nom de tous les actifs
            gse_market@Ticker_full_name = ticker_data[!(ticker_data$ticker == ""),1]

            return(gse_market)
        },
        error = function(e) {
            message("Make sure you have an active internet connection. ")
        },
        warning = function(w) {
            message("Make sure you have an active internet connection. ")
        }
    )
}






#' Récupérer les indices de la Bourse de Casablanca
#'
#' Cette fonction interroge l'API de la Bourse de Casablanca afin de récupérer
#' l'ensemble des indices (principaux, sectoriels, devises, etc.) sous forme
#' de dataframe structuré.
#'
#' Les données sont organisées par catégorie (title) et contiennent des
#' informations telles que la valeur de l'indice, la capitalisation,
#' les variations et les bornes (haut/bas).
#'
#' @param table_id Table ID.
#'
#' @return Un dataframe contenant :
#' \itemize{
#'   \item `title` : catégorie de l'indice (Main indices, Sector Indices, etc.)
#'   \item `index` : nom de l'indice
#'   \item `index_code` : code extrait de l'URL (ex: MASI, FCSELIQ)
#'   \item `field_index_value` : valeur actuelle de l'indice
#'   \item `field_var_year` : variation annuelle (%)
#'   \item `field_var_veille` : variation journalière (%)
#'   \item `field_market_capitalisation` : capitalisation
#'   \item `field_transact_time` : date/heure de la dernière transaction
#'   \item autres variables associées aux indices
#' }
#'
#' @details
#' La fonction :
#' \enumerate{
#'   \item Effectue une requête HTTP vers l'API BVC
#'   \item Parse le JSON retourné
#'   \item Aplati la structure imbriquée (`data -> items`)
#'   \item Ajoute la catégorie (`title`)
#'   \item Extrait le code de l'indice depuis `index_url`
#'   \item Remplace les valeurs NULL par NA
#'   \item Convertit les colonnes numériques et dates
#' }
#'
#' @examples
#' \dontrun{
#' df_indices <- get_bvc_index()
#' head(df_indices)
#' }
#'
#' @importFrom httr GET POST add_headers config timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate across select na_if everything
#' @importFrom purrr map map_dfr
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
gse_share_info = function(table_id = "34"){

    url_share = "https://gse.com.gh/wp-admin/admin-ajax.php"
    wdtnonce_id = get_gse_wdtnonce_id(url_for_wdtnonce = "https://gse.com.gh/listed-companies/",node = paste("wdtNonceFrontendEdit",table_id,sep = "_"))

    headers <- c(
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
        "sec-ch-ua" = "\"Chromium\";v=\"142\", \"Google Chrome\";v=\"142\", \"Not_A Brand\";v=\"99\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"Windows\""
    )

    data = list(
        draw = "1",
        `columns[0][data]` = "0",
        `columns[0][name]` = "wdt_ID",
        `columns[0][searchable]` = "true",
        `columns[0][orderable]` = "true",
        `columns[0][search][value]` = "",
        `columns[0][search][regex]` = "false",
        `columns[1][data]` = "1",
        `columns[1][name]` = "symbol",
        `columns[1][searchable]` = "true",
        `columns[1][orderable]` = "true",
        `columns[1][search][value]` = "",
        `columns[1][search][regex]` = "false",
        `columns[2][data]` = "2",
        `columns[2][name]` = "company",
        `columns[2][searchable]` = "true",
        `columns[2][orderable]` = "true",
        `columns[2][search][value]` = "",
        `columns[2][search][regex]` = "false",
        `columns[3][data]` = "3",
        `columns[3][name]` = "datelisted",
        `columns[3][searchable]` = "true",
        `columns[3][orderable]` = "true",
        `columns[3][search][value]` = "",
        `columns[3][search][regex]` = "false",
        `columns[4][data]` = "4",
        `columns[4][name]` = "statedcapital",
        `columns[4][searchable]` = "true",
        `columns[4][orderable]` = "true",
        `columns[4][search][value]` = "",
        `columns[4][search][regex]` = "false",
        `columns[5][data]` = "5",
        `columns[5][name]` = "issuedshares",
        `columns[5][searchable]` = "true",
        `columns[5][orderable]` = "true",
        `columns[5][search][value]` = "",
        `columns[5][search][regex]` = "false",
        `columns[6][data]` = "6",
        `columns[6][name]` = "authorisedshares",
        `columns[6][searchable]` = "true",
        `columns[6][orderable]` = "true",
        `columns[6][search][value]` = "",
        `columns[6][search][regex]` = "false",
        `order[0][column]` = "0",
        `order[0][dir]` = "asc",
        start = "0",
        length = "50",
        `search[value]` = "",
        `search[regex]` = "false",
        wdtNonce = wdtnonce_id
    )

    params = list(
        action = "get_wdtable",
        table_id = table_id
    )

    # Requête HTTP
    res <- try(POST(url_share,
                    query = params,
                   add_headers(.headers=headers),
                   timeout(30),
                   body = data,
                   encode = "form"), silent = TRUE)

    if (inherits(res, "try-error") || status_code(res) != 200) {
        message(paste("Can't recover the share"))
        return(NULL)
    }

    txt <- content(res, as = "text", encoding = "UTF-8")

    df <- as.data.frame(fromJSON(txt)$data, stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        setNames(c("id","ticker_html","company","listing_date","market_cap","price","shares")) %>%
        mutate(
            ticker = str_extract(ticker_html, "(?<=\\>)[^<]+"),
            ticker_url = str_extract(ticker_html, "(?<=href=')[^']+")
        ) %>%
        select(-ticker_html) %>%
        mutate(across(everything(), ~ na_if(.x, "")),Country.Code = "GH",Type = "Share")

    return(df)
}



#' Retrieve WDT Nonce Token from a Web Page
#'
#' @description
#' Internal helper function that retrieves a \code{wdtNonceFrontendEdit}
#' token from a specified web page. This token is typically required to perform
#' authenticated POST requests when querying server-side data tables.
#'
#' @details
#' The function sends an HTTP GET request to the provided URL, parses the returned
#' HTML content, and extracts the value of a hidden input field identified by its
#' HTML \code{id}.
#'
#' The nonce is dynamically generated by the server and must be included in
#' subsequent API requests (e.g., \code{admin-ajax.php}) to successfully retrieve
#' data.
#'
#' @param url_for_wdtnonce A character string specifying the URL of the web page
#' from which the nonce should be extracted.
#' @param node A character string representing the HTML \code{id} of the input
#' element containing the nonce (e.g., \code{"wdtNonceFrontendEdit_39"}).
#'
#' @return A character string representing the extracted nonce token. Returns
#' \code{NULL} if the request fails or if the node cannot be found.
#'
#' @keywords internal
#'
#' @importFrom httr GET add_headers config timeout status_code content
#' @importFrom rvest read_html html_node html_attr
#'
get_gse_wdtnonce_id = function(url_for_wdtnonce,node){

    headers <- c(
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
        "sec-ch-ua" = "\"Chromium\";v=\"142\", \"Google Chrome\";v=\"142\", \"Not_A Brand\";v=\"99\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"Windows\""
    )

    res <- try(GET(url_for_wdtnonce,
                   add_headers(.headers=headers),
                   config(ssl_verifypeer = TRUE),
                   timeout(30)), silent = TRUE)

    page <- read_html(content(res, "text", encoding = "UTF-8"))

    wdtNonce <- page %>%
        html_node(paste0("#",node)) %>%
        html_attr("value")

    return(wdtNonce)
}




#' Retrieve GSE Index and Share Information
#'
#' @description
#' This function extracts and formats ticker information from the Ghana Stock Exchange (GSE)
#' by calling \code{gse_share_info()}. It selects and returns a subset of relevant columns
#' corresponding to tickers, company names, and classification.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Calls \code{gse_share_info()} to retrieve raw data.
#'   \item Selects specific columns of interest.
#'   \item Returns the result as a clean data frame.
#' }
#'
#' @return A data frame containing selected GSE ticker information.
#'
#' @note
#' This function assumes that \code{gse_share_info()} returns a structured
#' data frame with consistent column ordering.
#'
#' @examples
#' \dontrun{
#' df <- get_gse_index_share()
#' head(df)
#' }
#'
get_gse_index_share = function(){

    df_index = data.frame(ticker = "GSE-CI",company = "GSE Composite Index",Country.Code = "GH",Type = "Index")
    df_share = rbind(gse_share_info("34"),gse_share_info("35"),gse_share_info("36"))
    df = rbind(df_index,df_share[,c(7,2,9,10)])

    return(as.data.frame(df))
}




#' Retrieve Historical Stock Data from GSE Web Portal
#'
#' @description
#' Internal function used to retrieve historical stock market data from the
#' Ghana Stock Exchange (GSE) web portal.
#'
#' The function sends POST requests to the GSE DataTables API endpoint,
#' extracts the returned JSON data, cleans and standardizes the results,
#' and formats the output into long or wide structures.
#'
#' It supports single or multiple tickers, flexible date ranges,
#' automatic request chunking, and multiple output formats.
#'
#' @param ticker A character vector of ticker symbols to retrieve.
#' Special values include:
#' \itemize{
#'   \item \code{"ALL"}: retrieve all available instruments
#'   \item \code{"ALL SHARES"}: retrieve only shares
#'   \item \code{"ALL INDEXES"}: retrieve only indexes
#' }
#'
#' @param Period A character string defining the data frequency.
#' Currently only \code{"daily"} is supported.
#'
#' @param from Start date (character or \code{Date}).
#' Default is 100 days before today.
#'
#' @param to End date (character or \code{Date}).
#' Default is today.
#'
#' @param output_format Output structure specification:
#' \itemize{
#'   \item \code{"by_col"}: long format with a \code{Ticker} column
#'   \item \code{"by_row"}: wide format with ticker-prefixed columns
#'   \item \code{"all"}: returns both formats as a named list
#' }
#'
#' @details
#' The function performs the following operations:
#'
#' \enumerate{
#'   \item Retrieves available GSE tickers using \code{GET_tickers("GSE")}.
#'
#'   \item Retrieves the required \code{wdtNonce} authentication token
#'   from the GSE trading portal.
#'
#'   \item Validates ticker symbols and requested date ranges.
#'
#'   \item Splits the requested period into chunks of approximately
#'   500 days to reduce request size and improve reliability.
#'
#'   \item Sends POST requests to the GSE DataTables AJAX endpoint.
#'
#'   \item Parses and standardizes the returned JSON data.
#'
#'   \item Keeps and formats the following variables:
#'   \code{Date}, \code{Open}, \code{Low},
#'   \code{High}, \code{Close}, and \code{Volume}.
#'
#'   \item Merges all ticker datasets into the requested output structure.
#' }
#'
#' A small random delay is introduced between requests in order to
#' reduce server load and limit request throttling.
#'
#' @return
#' Depending on \code{output_format}, returns:
#'
#' \itemize{
#'
#'   \item A long-format data frame (\code{"by_col"}) containing:
#'   \code{Date}, \code{Ticker}, \code{Open}, \code{High},
#'   \code{Low}, \code{Close}, and \code{Volume}.
#'
#'   \item A wide-format data frame (\code{"by_row"}) where each
#'   variable is prefixed by ticker names
#'   (e.g. \code{ACCESS.Close}).
#'
#'   \item A named list containing both formats when
#'   \code{output_format = "all"}.
#' }
#'
#' @section API Source:
#' Data are retrieved from the Ghana Stock Exchange web portal:
#' \url{https://gse.com.gh/}
#'
#' @section Limitations:
#' \itemize{
#'   \item Only daily frequency is currently supported.
#'
#'   \item The function depends on the internal structure of the
#'   GSE web portal and may stop working if the endpoint changes.
#'
#'   \item Historical availability depends on the GSE database.
#' }
#'
#' @section Error Handling:
#' \itemize{
#'   \item Invalid tickers are skipped with warning messages.
#'
#'   \item Failed HTTP requests are retried automatically.
#'
#'   \item The function stops if no valid data can be retrieved.
#' }
#'
#' @section Caching:
#' The latest successful download is stored inside \code{.pkg_env}
#' for reuse within the current R session.
#'
#' @examples
#' \dontrun{
#'
#' # Retrieve a single ticker
#' df <- .GET_data_GSE(
#'   ticker = "ACCESS",
#'   from = "2024-01-01",
#'   to = "2024-06-01"
#' )
#'
#' # Retrieve all shares
#' df_all <- .GET_data_GSE(
#'   ticker = "ALL SHARES"
#' )
#'
#' # Retrieve both output formats
#' df_both <- .GET_data_GSE(
#'   ticker = c("ACCESS", "CAL"),
#'   output_format = "all"
#' )
#' }
#'
#' @importFrom httr POST RETRY add_headers status_code content timeout
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate transmute distinct arrange bind_rows
#' @importFrom purrr pluck
#' @importFrom stats runif
#' @importFrom rlang abort
#'
#' @seealso
#' \code{\link{GET_tickers}},
#' \code{\link{output_data}}
#'
#' @keywords internal
.GET_data_GSE = function(ticker = "ALL",Period = "daily",from = Sys.Date() - 100,to = Sys.Date(),output_format = c("all","by_col","by_row")) {

    #ticker = "ACCESS"
    #tick = ticker

    # ====================================================
    max_retries = 5
    retry_delay = 3

    # ==================================================== Tickers
    market_code = "GSE"
    base_url = "https://gse.com.gh/"

    market_tickers = GET_tickers(toupper(market_code))
    wdtnonce_id = get_gse_wdtnonce_id(url_for_wdtnonce =  "https://gse.com.gh/trading-and-data/",node = "wdtNonceFrontendEdit_39")
    ticker <- unique(toupper(ticker))

    ifelse(ticker[1] =="ALL",ticker <- market_tickers@List,ticker)
    ifelse(ticker[1] =="ALL SHARES",ticker <- market_tickers@ListShares,ticker)
    ifelse(ticker[1] =="ALL INDEXES",ticker <- market_tickers@ListIndexes,ticker)

    # ==================================================== Date
    from <- as.Date(from)
    to <- as.Date(to)

    if (from >= to) {
        stop("'from' must be less than 'to'")
    }

    if(from > Sys.Date()) {
        stop("'from' must be less than 'today'")
    }

    periods = seq(from = from, to = to, by = "500 day")
    if(!(to %in% periods)){
        periods <- c(periods,to)
    }

    t_range = as.character(to - from + 1)

    # ==================================================== DATA BETWEEN PERIODS

    headers <- c(
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
        "sec-ch-ua" = "\"Chromium\";v=\"142\", \"Google Chrome\";v=\"142\", \"Not_A Brand\";v=\"99\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"Windows\""
    )

    params = list(
        action = "get_wdtable",
        table_id = "39"
    )


    df_tick = list()
    df_tickers = list()

    for (tick in ticker) {

        if (!(tick %in% market_tickers@List)) {
            message(paste("Can't find ticker",tick))
            next
        }

        df_period = list()
        df_test = NULL

        for(i in 1:(length(periods) - 1)) {  # parcourir les intervalles de periode

            session = active_client_session(url = base_url)

            from_date <- format(as.Date(periods[i]),"%d/%m/%Y")
            to_date <- format(as.Date(periods[i+1]),"%d/%m/%Y")

            data = list(
                draw = "4",
                `columns[0][data]` = "0",
                `columns[0][name]` = "wdt_ID",
                `columns[0][searchable]` = "true",
                `columns[0][orderable]` = "false",
                `columns[0][search][value]` = "",
                `columns[0][search][regex]` = "false",
                `columns[1][data]` = "1",
                `columns[1][name]` = "dailydate",
                `columns[1][searchable]` = "true",
                `columns[1][orderable]` = "true",
                `columns[1][search][value]` = paste(from_date,to_date,sep = "|"),
                `columns[1][search][regex]` = "false",
                `columns[2][data]` = "2",
                `columns[2][name]` = "sharecode",
                `columns[2][searchable]` = "true",
                `columns[2][orderable]` = "true",
                `columns[2][search][value]` = tick,
                `columns[2][search][regex]` = "true",
                `columns[3][data]` = "3",
                `columns[3][name]` = "yearhighgh",
                `columns[3][searchable]` = "true",
                `columns[3][orderable]` = "true",
                `columns[3][search][value]` = "",
                `columns[3][search][regex]` = "false",
                `columns[4][data]` = "4",
                `columns[4][name]` = "yearlowgh",
                `columns[4][searchable]` = "true",
                `columns[4][orderable]` = "true",
                `columns[4][search][value]` = "",
                `columns[4][search][regex]` = "false",
                `columns[5][data]` = "5",
                `columns[5][name]` = "previousclosingpricevwapgh",
                `columns[5][searchable]` = "true",
                `columns[5][orderable]` = "true",
                `columns[5][search][value]` = "",
                `columns[5][search][regex]` = "false",
                `columns[6][data]` = "6",
                `columns[6][name]` = "openingpricegh",
                `columns[6][searchable]` = "true",
                `columns[6][orderable]` = "true",
                `columns[6][search][value]` = "",
                `columns[6][search][regex]` = "false",
                `columns[7][data]` = "7",
                `columns[7][name]` = "lasttransactionpricegh",
                `columns[7][searchable]` = "true",
                `columns[7][orderable]` = "true",
                `columns[7][search][value]` = "",
                `columns[7][search][regex]` = "false",
                `columns[8][data]` = "8",
                `columns[8][name]` = "closingpricevwapgh",
                `columns[8][searchable]` = "true",
                `columns[8][orderable]` = "true",
                `columns[8][search][value]` = "",
                `columns[8][search][regex]` = "false",
                `columns[9][data]` = "9",
                `columns[9][name]` = "pricechangegh",
                `columns[9][searchable]` = "true",
                `columns[9][orderable]` = "true",
                `columns[9][search][value]` = "",
                `columns[9][search][regex]` = "false",
                `columns[10][data]` = "10",
                `columns[10][name]` = "closingbidpricegh",
                `columns[10][searchable]` = "true",
                `columns[10][orderable]` = "true",
                `columns[10][search][value]` = "",
                `columns[10][search][regex]` = "false",
                `columns[11][data]` = "11",
                `columns[11][name]` = "closingofferpricegh",
                `columns[11][searchable]` = "true",
                `columns[11][orderable]` = "true",
                `columns[11][search][value]` = "",
                `columns[11][search][regex]` = "false",
                `columns[12][data]` = "12",
                `columns[12][name]` = "totalsharestraded",
                `columns[12][searchable]` = "true",
                `columns[12][orderable]` = "true",
                `columns[12][search][value]` = "",
                `columns[12][search][regex]` = "false",
                `columns[13][data]` = "13",
                `columns[13][name]` = "totalvaluetradedgh",
                `columns[13][searchable]` = "true",
                `columns[13][orderable]` = "true",
                `columns[13][search][value]` = "",
                `columns[13][search][regex]` = "false",
                `order[0][column]` = "1",
                `order[0][dir]` = "desc",
                start = "0",
                length = t_range,
                `search[value]` = "",
                `search[regex]` = "false",
                wdtNonce = wdtnonce_id,
                sRangeSeparator = "|"
            )

            req <- tryCatch({
                httr::RETRY(
                    "POST",
                    url = "https://gse.com.gh/wp-admin/admin-ajax.php",
                    httr::add_headers(.headers = headers),
                    handle = session$handle,
                    query = params,
                    body = data,
                    encode = "form",
                    httr::timeout(retry_delay),
                    times = max_retries,
                    quiet = TRUE
                )

            }, error = function(e) NULL)


            Sys.sleep(runif(1,0.1,0.7))

            if (inherits(req, "try-error") || status_code(req) != 200) {
                message(paste("Can't extract data for",tick))
                next
            }


            df_test <- tryCatch(

                req %>%
                    content("text", encoding = "UTF-8") %>%
                    fromJSON() %>%
                    purrr::pluck("data") %>%
                    as.data.frame() %>%
                    setNames(c(
                        "Code","Date","Ticker","High","Low","PrevCloseVWAP",
                        "Open","LastPrice","Close","PriceChange",
                        "BidClose","AskClose","Volume","ValueTraded"
                    )) %>%

                    dplyr::transmute(
                        Date = as.Date(Date, format = "%d/%m/%Y"),
                        across(c(Open, Low, High, Close, Volume), ~ as.numeric(.x))
                    ),
                error = function(e) NULL
            )


            if(!is.null(df_test)) {
                df_period[[i]] = df_test
            } else {
                next
            }
        }


        if (length(df_period) == 0) {
            message(paste("[e]\u274C - Data not available for ticker",tick, "between :",from,"-",to))
            next
        } else {
            df_tick <- dplyr::bind_rows(df_period) %>%
                dplyr::distinct() %>%
                dplyr::arrange(Date)
            message("[100%]\u2705 - Data downloaded for ", tick," between ",min(df_tick$Date)," - ",max(df_tick$Date))

            df_tickers[[tick]] = df_tick
        }
    }

    if(length(df_tickers) == 0) {
        rlang::abort("No data available !")
    }

    df = output_data(data = df_tickers,output_format = output_format)

    if(.pkg_env$last_cache_operation_is_available == FALSE) {
        .pkg_env$last_data_downloaded = df
        .pkg_env$last_cache_operation_is_available = TRUE
    }

    return(df)
}


