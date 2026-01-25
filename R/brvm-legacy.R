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


