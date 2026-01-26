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

    options <- rvest::read_html("https://www.sikafinance.com/") %>%
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






#' Internal function to fetch historical data for BRVM securities
#'
#' @description
#' This internal function retrieves historical price and volume data for one or
#' more securities listed on the BRVM (Bourse Régionale des Valeurs Mobilières)
#' market via the Sika Finance API.
#'
#' @param ticker A character vector specifying the tickers to retrieve. Case-insensitive.
#' Special values include:
#' \itemize{
#'   \item \code{"ALL"}: All available securities.
#'   \item \code{"ALL SHARES"}: All listed stocks.
#'   \item \code{"ALL INDEXES"}: All market indexes.
#'   \item Specific vector: e.g., \code{c("ECOC", "SGBCI")}.
#' }
#' @param Period Data aggregation frequency. Valid values:
#' \itemize{
#'   \item \code{"daily"} or \code{0} (default)
#'   \item \code{"weekly"} or \code{7}
#'   \item \code{"monthly"} or \code{30}
#'   \item \code{"quarterly"} or \code{91}
#'   \item \code{"yearly"} or \code{365}
#' }
#' @param from Start date (\code{"YYYY-MM-DD"}).
#' Defaults to 89 days ago.
#' @param to End date (\code{"YYYY-MM-DD"}).
#' Defaults to the current date.
#' @param output_format Desired structure: \code{"by_col"} (long format, default)
#' or \code{"by_row"} (wide format).
#'
#' @details
#' The function fetches data in chunks of 89 days to handle API limitations and
#' ensures data continuity. It handles SSL verification issues and performs
#' auto-formatting of tickers using \code{toupper()}.
#'
#' \itemize{
#'   \item \code{"by_col"}: Single data frame with a \code{Ticker} column.
#'   \item \code{"by_row"}: Wide data frame where tickers prefix the column names
#'   (e.g., \code{SNTS.Close}).
#' }
#'
#' @return A data frame containing \code{Date}, \code{Open}, \code{High},
#' \code{Low}, \code{Close}, and \code{Volume}. If only one ticker is requested,
#' the \code{Ticker} column is omitted in \code{"by_col"} format.
#'
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
#'
#' @seealso \code{\link{.GET_tickers_BRVM}}
#'
#' @importFrom httr2 req_body_json req_perform request resp_body_json req_method req_headers
#' @importFrom dplyr group_by summarise as_tibble distinct arrange
#' @importFrom lubridate parse_date_time
#' @importFrom rlang abort
#' @importFrom stringr str_sub
#'
#' @keywords internal
.GET_data_BRVM = function(ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row")) {
        tryCatch({

            first_date <- lubridate::parse_date_time(from, orders = "ymd")
            end_date   <- lubridate::parse_date_time(to, orders = "ymd")
            ssl_verifypeer = TRUE

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

            tryCatch(
                {
                    if (!ssl_verifypeer) { # pour les ordinateurs qui n'ont pas le SSL a jours ou a jours par rapport aux ssl du serveur
                        message("This request is not protected.
                                    SSL verification is disabled.
                                    The request can be intercepted (MITM),
                                    data can be stolen or modified,
                                    and the host may be exposed to attacks.
                                    Avoid disabling SSL verification unless you fully trust the server and your internet connection.")
                    }

                    ticker <- unique(toupper(ticker))
                    market_tickers = .GET_tickers_BRVM()
                    all_tickers = market_tickers@List

                    ifelse(ticker =="ALL",ticker <- market_tickers@List,ticker)
                    ifelse(ticker =="ALL SHARES",ticker <- market_tickers@ListShares,ticker)
                    ifelse(ticker =="ALL INDEXES",ticker <- market_tickers@ListIndexes,ticker)

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

                    ticker_data <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
                    names(ticker_data) <- c("Date", "Open", "High", "Low", "Close", "Volume","Ticker")

                    nb_merging = 0 # nombre de fusion fait

                    if (as.numeric(Period) %in% c(0, 7, 30, 91, 365)){
                        #Tick = ticker[1]
                        #Period = 0
                        for (Tick in ticker) {

                            TickName = sub("\\..*", "", Tick)
                            asset_data <- as.data.frame(matrix(NA, ncol = 6, nrow = 0))
                            names(asset_data) <- c("Date", "Open", "High", "Low", "Close", "Volume")

                            range_period = seq(from = first_date, to = end_date, by = "89 day")
                            ifelse(!(end_date %in% range_period),
                                   range_period <- c(range_period,end_date),range_period)

                            range_period_length = length(range_period)
                            range_period_length_adjusted = range_period_length - 1 # parcourir jusqu'a l'avant derniere date

                            for(i in 1:range_period_length_adjusted) {  # parcourir les intervalles de periode

                                # i = 1
                                from_date <- as.Date.POSIXct(range_period[i])
                                to_date <- as.Date.POSIXct(range_period[i+1])

                                base_request <- request(market_tickers@Market_data_url) %>%
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

                                Sys.sleep(0.111) #;print("A") fast

                                my_data <- base_request %>%
                                    req_body_json(list('ticker'= Tick,
                                                       'datedeb'= from_date,
                                                       'datefin'= to_date,
                                                       'xperiod'= paste0(Period,''))) %>%
                                    req_perform() %>%
                                    resp_body_json(simplifyVector = T)

                                if (length(my_data$lst)==6) {
                                    my_data <- as.data.frame(my_data$lst)
                                    asset_data <- rbind(asset_data, my_data)
                                }
                            }

                            if (ncol(asset_data) == 6 && nrow(asset_data) > 0) {

                                asset_data$Date<-as.Date.character(asset_data$Date, format = "%d/%m/%Y")

                                ifelse (any(duplicated(asset_data$Date)),
                                        asset_data <- asset_data %>% dplyr::distinct(),asset_data)
                                # dplyr::group_by(Date)%>%
                                #     dplyr::summarise(Open = mean(Open),
                                #                      High = mean(High),
                                #                      Low = mean(Low),
                                #                      Close = mean(Close),
                                #                      Volume = mean(Volume))

                                message(paste0("\U2705 We obtained ",TickName,  " data from ",
                                               min(asset_data$Date),
                                               " to ",
                                               max(asset_data$Date)))

                                if(output_format[1] == "by_col"){ # agencer suivant les colonnes
                                    asset_data$Ticker <- TickName
                                    if (nb_merging == 0){
                                        ticker_data <- asset_data # initialisation
                                    } else if(nb_merging > 0){
                                        ticker_data <- rbind(ticker_data, asset_data)
                                    }
                                } else if(output_format[1] == "by_row"){ # agencer suivant les lignes

                                    asset_data_names = colnames(asset_data)
                                    colnames(asset_data) <- c(
                                        asset_data_names[1],
                                        paste(TickName,asset_data_names,sep = ".")[-1]
                                    )

                                    if (nb_merging == 0){
                                        ticker_data <- asset_data # initialisation
                                    } else if(nb_merging > 0){
                                        ticker_data <- merge(ticker_data, asset_data,by = asset_data_names[1],all = TRUE)
                                    }
                                }
                                nb_merging = nb_merging + 1

                            } else {
                                message(paste0("\u274C ",TickName," data aren't available between ",
                                               first_date,
                                               " and ",
                                               end_date))
                            }

                        }


                        if(is.data.frame(ticker_data)){
                            if (length(unique(ticker_data$Ticker)) == 1){
                                ticker_data = ticker_data[, -7] # enlever colonne ticker
                            }

                            ticker_data = ticker_data %>% dplyr::arrange(Date)
                        }

                        return(ticker_data) # final output

                    }

                    else {
                        message("Choose the best period between 0, 7, 30, 91 and 365")
                    }

                },
                error = function(e) {
                    message("Make sure you have an active internet connection")
                },
                warning = function(w) {
                    message("Make sure you have an active internet connection")
                }
            )
        })}
