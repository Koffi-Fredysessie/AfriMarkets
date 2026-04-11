#' Extraire le code interne d'un ticker depuis la Bourse de Casablanca
#'
#' Cette fonction permet de récupérer le code numérique interne (instrument ID)
#' associé à un ticker donné en interrogeant la page web correspondante sur
#' le site de la Bourse de Casablanca.
#'
#' Elle effectue une requête HTTP vers la page de l'instrument, parse le HTML,
#' puis extrait le code présent dans les attributs `href` des éléments dont
#' l'identifiant commence par "link-".
#'
#' @param ticker Un caractère (`character`). Le symbole du ticker tel qu'utilisé
#' sur le site de la Bourse de Casablanca.
#'
#' @return Un vecteur de caractères contenant le(s) code(s) numérique(s) extrait(s)
#' depuis les liens HTML. Retourne `NULL` si la requête échoue ou si la page
#' n'est pas accessible.
#'
#' @details
#' La fonction utilise les packages \code{httr} pour effectuer la requête HTTP
#' et \code{rvest} pour parser le contenu HTML.
#'
#' En cas d'échec de la requête (erreur ou code HTTP différent de 200),
#' un message est affiché et la fonction retourne `NULL`.
#'
#' @examples
#' \dontrun{
#' get_ticker_code("AFM")
#' }
#'
#' @importFrom httr GET add_headers config timeout status_code
#' @importFrom rvest read_html html_nodes html_attr
get_ticker_code = function(ticker){

    url_share_info <- paste("https://www.casablanca-bourse.com/fr/live-market/instruments",ticker,sep = "/")

    headers <- c(
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
        "sec-ch-ua" = "\"Chromium\";v=\"142\", \"Google Chrome\";v=\"142\", \"Not_A Brand\";v=\"99\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"Windows\""
    )

    # Requête HTTP
    res <- try(GET(url_share_info,
                   add_headers(.headers=headers),
                   config(ssl_verifypeer = TRUE),
                   timeout(30)), silent = TRUE)

    if (inherits(res, "try-error") || status_code(res) != 200) {
        message(paste("Can't recover the hidden index for",ticker))
        return(NULL)
    }

    # Lire la page
    page <- read_html(rawToChar(res$content))

    # Sélectionner les éléments dont l'id commence par "link-"
    nodes <- html_nodes(page, css = '[id^="link-"]')

    # Voir les attributs (ex: href)
    ticker_href = html_attr(nodes, "href")
    ticker_code = sub(".*instrument=([0-9]+).*", "\\1", ticker_href)

    return(ticker_code)
}






#' Récupération des données historiques des instruments de la Bourse de Casablanca
#'
#' Cette fonction permet d'extraire les données historiques de marché (prix, volume,
#' capitalisation, etc.) pour un ou plusieurs tickers depuis l'API de la Bourse de Casablanca.
#'
#' Elle prend en charge l'extraction pour un ticker spécifique ou l'ensemble des tickers disponibles,
#' sur une période donnée, et retourne les données sous forme de dataframe.
#'
#' @param ticker Un vecteur de caractères. Le(s) symbole(s) des tickers à extraire.
#' Par défaut "ALL" pour récupérer tous les tickers disponibles.
#'
#' @param Period Une chaîne de caractères indiquant la fréquence des données (non utilisé actuellement).
#' Valeur par défaut : "daily".
#'
#' @param from Date de début (classe Date ou chaîne YYYY-MM-DD).
#' Par défaut : aujourd'hui - 89 jours.
#'
#' @param to Date de fin (classe Date ou chaîne YYYY-MM-DD).
#' Par défaut : aujourd'hui.
#'
#' @param output_format Format de sortie des données :
#' \itemize{
#'   \item "by_col" : format long (chaque ligne correspond à une observation avec une colonne Ticker)
#'   \item "by_row" : format large (chaque ticker devient un préfixe de colonnes)
#' }
#'
#' @return Un dataframe contenant les données historiques des instruments :
#' \itemize{
#'   \item Date de transaction (`created`)
#'   \item Prix (ouverture, clôture, haut, bas)
#'   \item Volume et nombre de transactions
#'   \item Capitalisation et autres indicateurs
#' }
#'
#' @details
#' La fonction :
#' \enumerate{
#'   \item Récupère la liste des tickers disponibles via `GET_tickers()`
#'   \item Convertit les tickers en codes internes via `get_ticker_code()`
#'   \item Interroge l'API REST de la Bourse de Casablanca
#'   \item Agrège les données pour chaque ticker
#'   \item Nettoie les valeurs manquantes et convertit les types
#' }
#'
#' Un délai aléatoire (`Sys.sleep`) est introduit entre les requêtes pour éviter
#' les blocages côté serveur.
#'
#' @examples
#' \dontrun{
#' df <- .GET_data_BVC(ticker = "IAM")
#' df <- .GET_data_BVC(ticker = c("IAM", "BCP"))
#' df <- .GET_data_BVC(ticker = "ALL")
#' df <- .GET_data_BVC(ticker = c("IAM", "BCP"), output_format = "by_row")
#' }
#'
#' @importFrom httr GET add_headers config timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate select arrange bind_rows everything across where
#' @importFrom readr type_convert
#' @importFrom stats runif
.GET_data_BVC = function(ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("by_col","by_row")) {

    market_tickers = GET_tickers("BVC")

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

    df = NULL

    for (tick in ticker) {

        tick = toupper(tick)

        if (!(tick %in% market_tickers@List)) {
            message(paste("Can't find ticker",tick))
            next
        }


        ticker_code = get_ticker_code(tick)

        url_req = paste0(
            "https://www.casablanca-bourse.com/api/proxy/en/api/bourse_data/instrument_history?fields%5Binstrument_history%5D=symbol%2Ccreated%2CopeningPrice%2CcoursCourant%2ChighPrice%2ClowPrice%2CcumulTitresEchanges%2CcumulVolumeEchange%2CtotalTrades%2Ccapitalisation%2CcoursAjuste%2CclosingPrice%2CratioConsolide&fields%5Binstrument%5D=symbol%2ClibelleFR%2ClibelleAR%2ClibelleEN%2Cemetteur_url%2Cinstrument_url&fields%5Btaxonomy_term--bourse_emetteur%5D=name&include=symbol&sort%5Bdate-seance%5D%5Bpath%5D=created&sort%5Bdate-seance%5D%5Bdirection%5D=DESC&filter%5Bfilter-historique-instrument-emetteur%5D%5Bcondition%5D%5Bpath%5D=symbol.codeSociete.meta.drupal_internal__target_id&filter%5Bfilter-historique-instrument-emetteur%5D%5Bcondition%5D%5Bvalue%5D=-1&filter%5Bfilter-historique-instrument-emetteur%5D%5Bcondition%5D%5Boperator%5D=%3D&filter%5Binstrument-history-class%5D%5Bcondition%5D%5Bpath%5D=symbol.codeClasse.field_code&filter%5Binstrument-history-class%5D%5Bcondition%5D%5Bvalue%5D=1&filter%5Binstrument-history-class%5D%5Bcondition%5D%5Boperator%5D=%3D&filter%5Bpublished%5D=1&page%5Boffset%5D=0&filter%5Bfilter-date-start-vh%5D%5Bcondition%5D%5Bpath%5D=field_seance_date&filter%5Bfilter-date-start-vh%5D%5Bcondition%5D%5Boperator%5D=%3E%3D&filter%5Bfilter-date-start-vh%5D%5Bcondition%5D%5Bvalue%5D=",
            from,
            "&filter%5Bfilter-date-end-vh%5D%5Bcondition%5D%5Bpath%5D=field_seance_date&filter%5Bfilter-date-end-vh%5D%5Bcondition%5D%5Boperator%5D=%3C%3D&filter%5Bfilter-date-end-vh%5D%5Bcondition%5D%5Bvalue%5D=",
            to,
            "&filter%5Bfilter-historique-instrument-emetteur%5D%5Bcondition%5D%5Bpath%5D=symbol.meta.drupal_internal__target_id&filter%5Bfilter-historique-instrument-emetteur%5D%5Bcondition%5D%5Boperator%5D=%3D&filter%5Bfilter-historique-instrument-emetteur%5D%5Bcondition%5D%5Bvalue%5D=",
            ticker_code
        )

        # Requête HTTP
        res <- try(GET(url_req,
                       add_headers(.headers=headers),
                       config(ssl_verifypeer = TRUE),
                       timeout(30)), silent = TRUE)

        Sys.sleep(runif(1,0.1,0.5))

        if (inherits(res, "try-error") || status_code(res) != 200) {
            message(paste("Can't extract data for",tick))
            next
        }

        pre_df_tick <- res %>%
            content("text", encoding = "UTF-8") %>%
            fromJSON() %>%
            .$data %>%
            .$attributes %>%
            bind_rows()

        if(nrow(pre_df_tick) == 0){
            message(paste("[e] - Data not available for ticker",tick, "between :",from,"-",to))
            next
        } else {
            message(paste("[100%] - Data extraction for ticker",tick, "between :",from,"-",to))
        }

        if(output_format[1] == "by_col") {
            df_tick = pre_df_tick %>%
                mutate(Ticker = tick) %>%
                select(created, Ticker, everything()) %>%
                arrange(created)

            if(is.null(df)){
                df = df_tick
            } else {
                df = rbind(df,df_tick)
                rm(pre_df_tick) ; rm(df_tick)
            }

        } else if(output_format[1] == "by_row") {

            df_tick = pre_df_tick %>%
                arrange(created) %>%
                select(created, everything())

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
        mutate(created = as.Date(created, format = "%Y-%m-%d")) %>%
        type_convert()

    return(df)

}



