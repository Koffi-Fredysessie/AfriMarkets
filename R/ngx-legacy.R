
#' Retrieve NGX Shares and Index Metadata
#'
#' @description
#' Internal function that retrieves metadata for both listed companies (shares)
#' and market indices from the Nigerian Exchange (NGX).
#'
#' The function combines data from two sources:
#' \itemize{
#'   \item A REST API endpoint providing structured information about listed companies.
#'   \item A web page containing index definitions, scraped from HTML.
#' }
#'
#' @details
#' \strong{Shares:}
#' Company-level data is fetched from the NGX REST API endpoint:
#' \url{https://doclib.ngxgroup.com/REST/api/issuers/companydirectory}
#'
#' The returned dataset includes company information such as:
#' \itemize{
#'   \item Company name
#'   \item Symbol (ticker)
#'   \item Sector classification
#'   \item Market capitalization
#'   \item Shares outstanding
#' }
#'
#' \strong{Indices:}
#' Index metadata is extracted from the NGX website:
#' \url{https://ngxgroup.com/exchange/data/indices/}
#'
#' The function parses the HTML content to retrieve:
#' \itemize{
#'   \item Index code (\code{value})
#'   \item Index name (\code{label})
#'   \item Description (\code{data-description})
#' }
#'
#' Cookies and headers are set to mimic a real browser request in order to
#' ensure proper server responses.
#'
#' @return
#' A named list containing two data frames:
#' \itemize{
#'   \item \code{Index}: A data frame with columns \code{value}, \code{label},
#'   \code{description}, and \code{Type = "Index"}.
#'   \item \code{Share}: A data frame containing company-level information
#'   with additional columns \code{Type = "Share"} and \code{Country.code = "NG"}.
#' }
#'
#' @keywords internal
#'
#' @importFrom httr GET add_headers content set_cookies
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
#' @importFrom rvest read_html html_node html_nodes html_attr html_text
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @examples
#' \dontrun{
#' data <- ngx_share_index_info()
#'
#' # Access shares
#' shares <- data$Share
#'
#' # Access indices
#' indices <- data$Index
#' }
ngx_share_index_info = function(){

    cookies = c(
        `cookielawinfo-checkbox-necessary` = "yes",
        `cookielawinfo-checkbox-non-necessary` = "yes"
    )

    headers = c(
        accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        `accept-language` = "en-US,en;q=0.9,fr-FR;q=0.8,fr;q=0.7",
        `cache-control` = "max-age=0",
        `if-modified-since` = "Sat, 18 Apr 2026 11:26:31 GMT",
        `if-none-match` = '"06b7f03e6ab83c7765c5def34411d9f9"',
        priority = "u=0, i",
        referer = "https://ngxgroup.com/exchange/trade/equities/listed-companies/",
        `sec-ch-ua` = '"Google Chrome";v="147", "Not.A/Brand";v="8", "Chromium";v="147"',
        `sec-ch-ua-mobile` = "?0",
        `sec-ch-ua-platform` = '"Windows"',
        `sec-fetch-dest` = "document",
        `sec-fetch-mode` = "navigate",
        `sec-fetch-site` = "same-origin",
        `sec-fetch-user` = "?1",
        `upgrade-insecure-requests` = "1",
        `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36"
    )

    # share

    res <- httr::GET(url = "https://doclib.ngxgroup.com/REST/api/issuers/companydirectory?$orderby=CompanyName", httr::add_headers(.headers=headers))
    txt <- content(res, as = "text", encoding = "UTF-8")

    df_share <- fromJSON(txt) %>%
        mutate(Type = "Share",Country.code = "NG")

    #Index

    res <- httr::GET(url = "https://ngxgroup.com/exchange/data/indices/", httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies))
    page <- read_html(content(res, "text", encoding = "UTF-8"))
    df_index <- page %>%
        html_node("#chartIndices") %>%
        html_nodes("option") %>%
        {
            data.frame(
                value = html_attr(., "value"),
                label = html_text(., trim = TRUE),
                description = html_attr(., "data-description"),
                stringsAsFactors = FALSE
            )
        } %>%
        mutate(Type = "Index")

    return(
        list(
            Index = df_index,
            Share = df_share
        )
    )
}


#' Retrieve NGX Shares and Indexes Metadata
#'
#' @description
#' This function retrieves and formats metadata for both equities (shares)
#' and market indexes listed on the Nigerian Exchange (NGX).
#'
#' It internally calls \code{ngx_share_index_info()} to collect raw data from:
#' \itemize{
#'   \item The NGX company directory API (for listed companies)
#'   \item The NGX website (for available market indexes)
#' }
#'
#' The function then standardizes and returns the data into two clean tables:
#' one for indexes and one for shares.
#'
#' @details
#' \strong{Returned Structures:}
#' \itemize{
#'   \item \strong{Index}: Contains available NGX indexes with their ticker codes
#'   and display names.
#'   \item \strong{Share}: Contains listed companies with their ticker symbols,
#'   company names, sectors, and business descriptions.
#' }
#'
#' The output is designed to be directly usable with other AfriMarkets
#' functions such as \code{GET_data()}.
#'
#' @return A named list with two elements:
#' \itemize{
#'   \item \code{Index}: A data frame with columns \code{Ticker}, \code{Name}, and \code{Type}
#'   \item \code{Share}: A data frame with columns \code{Ticker}, \code{Name},
#'   \code{Sector}, \code{NatureofBusiness}, and \code{Type}
#' }
#'
#' @family Data Retrieval
#'
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
#'
#' @examples
#' \dontrun{
#' # Retrieve NGX shares and indexes
#' ngx_data <- get_ngx_share_index()
#'
#' # View indexes
#' head(ngx_data$Index)
#'
#' # View shares
#' head(ngx_data$Share)
#' }
#'
get_ngx_share_index = function(){
    ngx_info = ngx_share_index_info()

    return(
        list(
            Index = ngx_info$Index %>% select(value,label,Type) %>% rename(Ticker = value,Name = label),
            Share = ngx_info$Share %>% select(Symbol,CompanyName,Sector,NatureofBusiness,Type,Country.code) %>% rename(Ticker = Symbol,Name = CompanyName)
        )
    )
}




#' Retrieve NGX MARKET Object with Available Tickers
#'
#' @description
#' Internal function that retrieves the Nigerian Exchange (NGX)
#' market object populated with the latest available tickers for
#' both shares and indexes.
#'
#' The function calls \code{get_ngx_share_index()} to fetch raw metadata and
#' formats the results into the standardized \code{MARKET} S4 object
#' used throughout the AfriMarkets package.
#'
#' @details
#' The process is performed in three main steps:
#' \enumerate{
#'   \item Retrieve raw data for NGX shares and indexes.
#'   \item Filter and structure the data based on asset type (\code{"Share"} and \code{"Index"}).
#'   \item Populate the slots of the \code{MARKET} object:
#'   \itemize{
#'     \item \code{Indexes}: Data frame of index tickers and names
#'     \item \code{Shares}: Data frame of share tickers and associated metadata
#'     \item \code{ListIndexes}: Vector of index tickers
#'     \item \code{ListShares}: Vector of share tickers
#'     \item \code{List}: Combined vector of all tickers
#'     \item \code{Ticker_full_name}: Full list of tickers for lookup
#'   }
#' }
#'
#' @return
#' An object of class \code{MARKET} corresponding to the NGX exchange.
#' Returns \code{NULL} if the data cannot be retrieved (e.g., no internet connection).
#'
#' @note
#' This is an internal function (prefixed with \code{"."}) and is not intended
#' to be called directly by end users.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
#' @author Koffi Frederic SESSIE
#' @author Olabiyi Aurel Geoffroy ODJO
.GET_tickers_NGX = function() {
    tryCatch(
        {

            # General extraction

            ticker_data = get_ngx_share_index()

            # MISE EN FORMAT SELON L'AFFICHAGE STANDARD

            NGX_market = CREATE_ALL_MARKETS()$NGX_MARKET

            # Indexes
            NGX_market@Indexes = ticker_data$Index[ticker_data$Index$Type == "Index",1:2]
            rownames(NGX_market@Indexes) = NULL

            # Shares
            NGX_market@Shares = ticker_data$Share[ticker_data$Share$Type == "Share",c(1:3,6)]
            rownames(NGX_market@Shares) = NULL

            # Name List
            NGX_market@ListIndexes = NGX_market@Indexes[,1]
            NGX_market@ListShares = NGX_market@Shares[,1]
            NGX_market@List = c(NGX_market@ListIndexes,NGX_market@ListShares) #Nom de tous les actifs
            NGX_market@Ticker_full_name = NGX_market@List

            return(NGX_market)
        },
        error = function(e) {
            print(e)
            message("Make sure you have an active internet connection. ")
        },
        warning = function(w) {
            message("Make sure you have an active internet connection. ")
        }
    )
}


