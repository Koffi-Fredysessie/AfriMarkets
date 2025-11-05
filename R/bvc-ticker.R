
#' @title Get BVC Tickers
#'
#' @description A method that retrieves and processes ticker data for indexes and shares from the BVC website.
#' Retrieves information about companies listed on the Bourse de Casablanca (BVC).
#' This function returns an S4 object containing the list of tickers, detailed shares information,
#' and (optionally) BVC indexes.
#'
#' @param object An object of class `missing`, indicating this method is a constructor that does not require an input object.
#' @return A new S4 object of class `market_description` populated with ticker data from the BVC.
#'
#' @details
#' The BVC (Bourse de Casablanca) is a regional stock exchange serving the
#' West African Economic and Monetary Union (WAEMU / UEMOA).
#'
#' @importFrom methods setGeneric setMethod
#' @import rvest
#' @import httr2
#' @import tidyr
#'
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
#' @author Oudouss Diakite Abdoul
#' @author Steven P. Sanderson II, MPH
#'
#' @examples
#'\dontrun{
#' BVC_tickers()
#' ticks <- BVC_tickers()
#' dput(ticks$Ticker) ## Returns the name of all tickers
#'}
#'
#' \donttest{
#' # Retrieve BVC tickers
#' library(rvest)
#' library(httr2)
#' library(tidyr)
#' BVC_tickers <- BVC_tickers()
#'
#' # Display shares
#' BVC_tickers$Shares
#'
#' # List of tickers
#' BVC_tickers$List
#'
#' # Print object
#' BVC_tickers
#' }
#'
#' @rdname BVC_tickers
#' @export
setGeneric("BVC_tickers", function(object) standardGeneric("BVC_tickers" ))


#' @rdname BVC_tickers
#' @export
setMethod("BVC_tickers", signature(object = "missing"), function(object) {
    tryCatch(
        {
            # url_indexes = "https://www.BVC.org/en/indices"
            # url_shares = "https://www.BVC.org/en/cours-actions/0/"
            #
            # # indexes
            # object@Indexes = data.frame(Ticker = paste0(
            #     "BVC",c(
            #         "30","C","PR","PA","-CB","-CD","-EN","-IN","SF","SP","-TEL","AG","AS",
            #         "DI","FI","IN","-SP","TR"
            #     )
            # ),
            # Name = c('BVC - 30','BVC - COMPOSITE','BVC - PRESTIGE','BVC - PRINCIPAL','BVC - CONSOMMATION DE BASE','BVC - CONSOMMATION DISCRETIONNAIRE','BVC - ENERGIE','BVC - INDUSTRIELS','BVC - SERVICES FINANCIERS','BVC - SERVICES PUBLICS','BVC - TELECOMMUNICATIONS','BVC - AGRICULTURE','BVC - AUTRES SECTEURS','BVC - DISTRIBUTION','BVC - FINANCE','BVC - INDUSTRIE','BVC - SERVICES PUBLICS','BVC - TRANSPORT')
            # )
            #
            # #indexes_page <- GET(url_indexes, config(ssl_verifypeer = FALSE))
            # #indexes_tables <- read_html(indexes_page, encoding = "UTF-8") %>%
            # #html_elements("table") %>% html_table()
            # #object@Indexes = as.data.frame(do.call("rbind",indexes_tables[4:100]))[1]
            #
            # # shares
            # asset_page <- GET(url_shares, config(ssl_verifypeer = FALSE))
            # asset_tables <- read_html(asset_page, encoding = "UTF-8") %>%
            #     html_elements("table") %>% html_table()
            # object@Shares = as.data.frame(asset_tables[[4]])[1:2]
            # colnames(object@Shares)<-c("Ticker","Company name")
            #
            # # List
            # object@List = c(object@Indexes[,1],object@Shares[,1])

            BVC_market = CREATE_ALL_MARKETS()$BVC_MARKET

            #
            # # Extraire les éléments <option> dans le <select id="dpShares">
            # options <- rvest::read_html(BVC_market@Market_url[1]) %>%
            #     rvest::html_element(xpath = '//*[@id="dpShares"]') %>%
            #     rvest::html_elements("option")
            #

            # General extraction

            ticker_data <- data.frame(
                Type = ifelse(options %>% html_attr("value") == "","Nothing",
                              ifelse(grepl("\\.",options %>% html_attr("value")),"Share","Index")),
                Ticker = options %>% html_attr("value") %>% strsplit("\\.") %>% sapply(`[`, 1), # avant les "."
                Description = options %>% html_text(trim = TRUE),
                `Country.Code` = options %>% html_attr("value") %>% strsplit("\\.") %>% sapply(`[`, 2) %>% toupper(),
                Ticker_fullname = options %>% html_attr("value")
            )
            ticker_data = ticker_data[order(ticker_data$Country.Code ),]


            # Indexes
            BVC_market@Indexes = ticker_data[startsWith(ticker_data$Ticker_fullname,"BVC") & ticker_data$Type == "Index",2:3]
            rownames(BVC_market@Indexes) = NULL

            # Shares
            BVC_market@Shares = ticker_data[!startsWith(ticker_data$Ticker_fullname,"BVC") & !startsWith(ticker_data$Ticker_fullname,"SIKA") & ticker_data$Type == "Share",2:4]
            rownames(BVC_market@Shares) = NULL

            # Name List
            BVC_market@ListIndexes = BVC_market@Indexes[,1]
            BVC_market@ListShares = BVC_market@Shares[,1]
            BVC_market@List = c(BVC_market@ListIndexes,BVC_market@ListShares) #Nom de tous les actifs
            BVC_market@Ticker_full_name = ticker_data[!(ticker_data$Ticker_fullname == ""),5]

            return(BVC_market)
        },
        error = function(e) {
            message("Make sure you have an active internet connection. ")
        },
        warning = function(w) {
            message("Make sure you have an active internet connection. ")
        }
    )
})

