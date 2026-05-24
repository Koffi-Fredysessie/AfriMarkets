#' Mettre à jour l'objet Market avec les tickers de la BRVM
#'
#' @description
#' Cette fonction interne récupère les données de la BRVM via \code{brvm_share_index_info()},
#' les filtre pour exclure les tickers non pertinents (comme ceux de Sika Finance),
#' et organise les résultats dans une structure d'objet Market dédiée.
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \enumerate{
#'   \item Récupération des données brutes.
#'   \item Tri par code pays.
#'   \item Filtrage des indices commençant par "BRVM".
#'   \item Filtrage des actions (en excluant les préfixes "BRVM" et "SIKA").
#'   \item Mise à jour des slots de l'objet S4 (Indexes, Shares, List, etc.).
#' }
#'
#' @return Un objet de classe \code{MARKET} (tel que défini par \code{CREATE_ALL_MARKETS})
#' contenant les listes d'actifs mises à jour. En cas d'échec de connexion,
#' affiche un message d'erreur.
#'
#' @note
#' Cette fonction est destinée à un usage interne. Elle utilise un bloc \code{tryCatch}
#' pour gérer les interruptions de connexion internet.
#'
#' @importFrom methods new
#'
#' @keywords internal
.GET_tickers_BRVM = function() {
    tryCatch(
        {
            # General extraction

            ticker_data = brvm_share_index_info()
            ticker_data = ticker_data[order(ticker_data$Country.Code ),]


            # MISE EN FORMAT SELON L'AFFICHAGE STANDARD
            brvm_market = CREATE_ALL_MARKETS()$BRVM_MARKET

            # Indexes
            brvm_market@Indexes = ticker_data[startsWith(ticker_data$Ticker_fullname,"BRVM") & ticker_data$Type == "Index",2:3]
            rownames(brvm_market@Indexes) = NULL

            # Shares
            brvm_market@Shares = ticker_data[!startsWith(ticker_data$Ticker_fullname,"BRVM") & !startsWith(ticker_data$Ticker_fullname,"SIKA") & ticker_data$Type == "Share",2:4]
            rownames(brvm_market@Shares) = NULL

            # Name List
            brvm_market@ListIndexes = brvm_market@Indexes[,1]
            brvm_market@ListShares = brvm_market@Shares[,1]
            brvm_market@List = c(brvm_market@ListIndexes,brvm_market@ListShares) #Nom de tous les actifs
            brvm_market@Ticker_full_name = ticker_data[!(ticker_data$Ticker_fullname == ""),5]

            return(brvm_market)
        },
        error = function(e) {
            message("Make sure you have an active internet connection. ")
        },
        warning = function(w) {
            message("Make sure you have an active internet connection. ")
        }
    )
}



#' Récupérer les informations sur les actions et indices de la BRVM
#'
#' @description
#' Cette fonction extrait la liste des actions et des indices boursiers disponibles
#' sur Sika Finance. Elle traite les données brutes pour séparer les tickers,
#' les codes pays et les descriptions.
#'
#' @details
#' La fonction utilise le web scraping pour récupérer le contenu de la balise
#' sélection `dpShares` sur la page d'accueil de Sika Finance. Elle catégorise
#' ensuite chaque entrée comme "Share" (Action), "Index" (Indice) ou "Nothing".
#'
#' @return Un data.frame contenant les colonnes suivantes :
#' \itemize{
#'   \item \strong{Type}: Catégorie de l'élément (Share, Index ou Nothing).
#'   \item \strong{Ticker}: Le symbole boursier (ex: SNTS).
#'   \item \strong{Description}: Le nom complet de l'entreprise ou de l'indice.
#'   \item \strong{Country.Code}: Le code pays en majuscules (ex: SN, CI).
#'   \item \strong{Ticker_fullname}: La valeur brute combinée (ex: SNTS.SN).
#' }
#'
#' @importFrom rvest read_html html_element html_elements html_attr html_text
#' @importFrom magrittr %>%
#' @import rvest
#' @import httr2
#' @import tidyr
#'
#'
#' @examples
#' \dontrun{
#'  df_brvm <- brvm_share_index_info()
#'  head(df_brvm)
#' }
brvm_share_index_info = function(){

    # Extraire les éléments <option> dans le <select id="dpShares">
    session = active_client_session(url = "https://www.sikafinance.com/")

    options <- rvest::read_html(session$basic_req$content) %>%
        rvest::html_element(xpath = '//*[@id="dpShares"]') %>%
        rvest::html_elements("option")

    # General extraction

    ticker_data <- data.frame(
        Type = ifelse(options %>% html_attr("value") == "","Nothing",
                      ifelse(grepl("\\.",options %>% html_attr("value")),"Share","Index")),


        Ticker = options %>% html_attr("value") %>% strsplit("\\.") %>% sapply(`[`, 1),


        Description = options %>% html_text(trim = TRUE),


        `Country.Code` = options %>% html_attr("value") %>% strsplit("\\.") %>% sapply(`[`, 2) %>% toupper(),


        Ticker_fullname = options %>% html_attr("value")
    )

    return(ticker_data)
}





.GET_data_BRVM = function(ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("all","by_col","by_row")) {

     # tick = "BOAB"

    # ====================================================
    max_retries = 5
    retry_delay = 3

    # ==================================================== Tickers
    base_url = "https://www.sikafinance.com"

    market_tickers = GET_tickers("BRVM")
    ticker <- unique(toupper(ticker))

    ifelse(ticker[1] =="ALL",ticker <- market_tickers@List,ticker)
    ifelse(ticker[1] =="ALL SHARES",ticker <- market_tickers@ListShares,ticker)
    ifelse(ticker[1] =="ALL INDEXES",ticker <- market_tickers@ListIndexes,ticker)

    tick_vec = NULL
    full_ticker_name = market_tickers@Ticker_full_name

    for (tick in ticker) {
        locate_ticker = startsWith(full_ticker_name,tick)
        if(any(locate_ticker)){
            tick_vec = c(tick_vec,full_ticker_name[which(locate_ticker)])
        } else {
            message(paste("Ticker",tick,"not found. Make sure it exists !"))
        }
    }

    # ==================================================== Date
    from <- as.Date(from)
    to <- as.Date(to)

    if (from >= to) {
        stop("'from' must be less than 'to'")
    }

    if(from > Sys.Date()) {
        stop("'from' must be less than 'today'")
    }

    periods = seq(from = from, to = to, by = "89 day")
    if(!(to %in% periods)){
        periods <- c(periods,to)
    }

    #t_range = as.character(to - from + 1)

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

    if (!(as.numeric(Period) %in% c(0, 7, 30, 91, 365))) {
        rlang::abort("Period not found !")
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

    df_tick = list()
    df_tickers = list()

    for (tick in ticker) {

        TickName = sub("\\..*", "", tick)
        df_period = list()
        df_test = NULL

        for(i in 1:(length(periods) - 1)) {  # parcourir les intervalles de periode

            session = active_client_session(url = base_url)
            from_date <- periods[i]
            to_date <- periods[i+1]

            headers = c(
                accept = "*/*",
                `accept-language` = "en-US,en;q=0.9,fr-FR;q=0.8,fr;q=0.7",
                `content-type` = "application/json;charset=UTF-8",
                origin = base_url,
                priority = "u=1, i",
                referer = paste0(base_url,"/marches/historiques/",tick),
                `sec-ch-ua` = '"Google Chrome";v="147", "Not.A/Brand";v="8", "Chromium";v="147"',
                `sec-ch-ua-mobile` = "?0",
                `sec-ch-ua-platform` = '"Windows"',
                `sec-fetch-dest` = "empty",
                `sec-fetch-mode` = "cors",
                `sec-fetch-site` = "same-origin",
                `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36"
            )

            Sys.sleep(runif(1,0,0.5))

            data = paste0('{"ticker":"',tick,'","datedeb":"',from_date,'","datefin":"',to_date,'","xperiod":"',Period,'"}')

            req <- tryCatch({
                httr::RETRY(
                    "POST",
                    url = "https://www.sikafinance.com/api/general/GetHistos",
                    httr::add_headers(.headers = headers),
                    handle = session$handle,
                    body = data,
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
                    purrr::pluck("lst") %>%
                    as.data.frame() %>%
                    dplyr::transmute(
                        Date   = as.Date(Date, format = "%d/%m/%Y"),
                        Open   = as.numeric(Open),
                        High   = as.numeric(High),
                        Low    = as.numeric(Low),
                        Close  = as.numeric(Close),
                        Volume = as.numeric(Volume)
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
            message(paste("[e]\u274C - Data not available for ticker",TickName, "between :",from,"-",to))
            next
        } else {
            df_tick <- dplyr::bind_rows(df_period) %>%
                dplyr::distinct() %>%
                dplyr::arrange(Date)
            message("[100%]\u2705 - Data downloaded for ", TickName," between ",min(df_tick$Date)," - ",max(df_tick$Date))

            df_tickers[[TickName]] = df_tick
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
