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
            gse_market@Indexes = data.frame(NA) #ticker_data[startsWith(ticker_data$Ticker_fullname,"BRVM") & ticker_data$Type == "Index",2:3]
            #rownames(gse_market@Indexes) = NULL

            # Shares
            gse_market@Shares = ticker_data[ticker_data$Type == "Share",1:3]
            rownames(gse_market@Shares) = NULL

            # Name List
            gse_market@ListIndexes = "" #gse_market@Indexes[,1]
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
#' @importFrom httr GET add_headers config timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate across
#' @importFrom purrr map map_dfr
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
gse_share_info = function(){

    url_share = "https://gsewebportal.com/wp-admin/admin-ajax.php"

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
        wdtNonce = "5f9dab07a2"
    )

    params = list(
        action = "get_wdtable",
        table_id = "34"
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

    df = gse_share_info()
    df = df[,c(7,2,9,10)]

    return(as.data.frame(df))
}





#' Retrieve Historical Stock Data from GSE Web Portal
#'
#' @description
#' This function extracts historical daily stock market data from the Ghana Stock Exchange (GSE)
#' web portal for one or multiple tickers. It sends a POST request to the GSE DataTables API,
#' retrieves the raw JSON response, and formats it into a clean data frame.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Retrieves the list of available tickers using \code{GET_tickers("GSE")}.
#'   \item Filters input tickers or uses all available tickers when \code{"ALL"} is specified.
#'   \item Sends an HTTP POST request to the GSE web portal DataTables endpoint.
#'   \item Extracts and parses JSON response data.
#'   \item Renames and standardizes column names.
#'   \item Optionally reshapes output either by column or by row.
#' }
#'
#' @param ticker Character vector of stock tickers. Use \code{"ALL"} to retrieve all available tickers.
#' @param Period Character string defining data frequency (currently not used, default is "daily").
#' @param from Start date for data extraction (Date or character).
#' @param to End date for data extraction (Date or character).
#' @param output_format Output structure: \code{"by_col"} (default) for long format,
#' or \code{"by_row"} for wide format.
#'
#' @return A data frame containing historical stock market data with standardized columns:
#' Date, Ticker, YearHigh, YearLow, PrevCloseVWAP, OpenPrice, LastPrice,
#' CloseVWAP, PriceChange, BidClose, AskClose, Volume, ValueTraded.
#'
#' @note
#' This function relies on a private web endpoint of the GSE portal and may break if
#' the underlying HTML structure or API changes. It includes retry handling and basic
#' error messages for unavailable tickers.
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate across select arrange
#' @importFrom purrr map
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @importFrom readr type_convert
#' @importFrom stats runif
#'
#' @keywords internal
.GET_data_BVC = function(ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row")) {

    market_tickers = GET_tickers("GSE")

    ifelse(ticker[1] =="ALL",ticker <- market_tickers@List,ticker)
    ifelse(ticker[1] =="ALL SHARES",ticker <- market_tickers@ListShares,ticker)
    ifelse(ticker[1] =="ALL INDEXES",ticker <- market_tickers@ListIndexes,ticker)


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

    df = NULL

    tick = ticker[1]
    for (tick in ticker) {

        tick = toupper(tick)

        if (!(tick %in% market_tickers@List)) {
            message(paste("Can't find ticker",tick))
            next
        }

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
            `columns[1][search][value]` = paste(format(as.Date(from),"%d/%m/%Y"),format(as.Date(to),"%d/%m/%Y"),sep = "|"),
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
            length = "25",
            `search[value]` = "",
            `search[regex]` = "false",
            wdtNonce = "809010a2eb",
            sRangeSeparator = "|"
        )

        res <- POST(url = "https://gsewebportal.com/wp-admin/admin-ajax.php",
                    add_headers(.headers=headers),
                    query = params,
                    body = data, encode = "form")

        Sys.sleep(runif(1,0.1,0.5))

        if (inherits(res, "try-error") || status_code(res) != 200) {
            message(paste("Can't extract data for",tick))
            next
        }

        pre_df_tick <- res %>%
            content("text", encoding = "UTF-8") %>%
            fromJSON() %>%
            .$data %>%
            as.data.frame()


        if(nrow(pre_df_tick) == 0){
            message(paste("[e] - Data not available for ticker",tick, "between :",from,"-",to))
            next
        } else {
            message(paste("[100%] - Data extraction for ticker",tick, "between :",from,"-",to))
        }

        colnames(pre_df_tick) <- c(
            "Code",
            "Date",
            "Ticker",
            "YearHigh",
            "YearLow",
            "PrevCloseVWAP",
            "OpenPrice",
            "LastPrice",
            "CloseVWAP",
            "PriceChange",
            "BidClose",
            "AskClose",
            "Volume",
            "ValueTraded"
        )

        if(output_format[1] == "by_col") {
            df_tick = pre_df_tick %>%
                mutate(Ticker = tick) %>%
                select(Date, Ticker, everything()) %>%
                arrange(Date)

            if(is.null(df)){
                df = df_tick
            } else {
                df = rbind(df,df_tick)
                rm(pre_df_tick) ; rm(df_tick)
            }

        } else if(output_format[1] == "by_row") {

            df_tick = pre_df_tick %>%
                arrange(Date) %>%
                select(Date, everything())

            colnames(df_tick) = c(colnames(df_tick)[1],paste(tick,colnames(df_tick)[-1],sep = "."))

            if(is.null(df)){
                df = df_tick
            } else {
                df = merge(x= df,y= df_tick,by = colnames(df_tick)[1],all = TRUE)
                rm(pre_df_tick) ; rm(df_tick)
            }
        }

    }

    df = df %>%
        mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
        type_convert()

    return(df)

}


