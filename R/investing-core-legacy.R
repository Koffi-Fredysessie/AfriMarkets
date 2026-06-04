
#' Retrieve Market Identifiers from Investing.com
#'
#' This function scrapes the indices page from Investing.com and extracts
#' market identifiers and metadata from the embedded JSON structure.
#' It parses the \code{__NEXT_DATA__} script content and retrieves
#' country-related market information.
#'
#' The function returns a cleaned data frame containing unique market entries,
#' including identifiers, names, flags, and links.
#'
#' @param base_url A character string representing the base URL of Investing.com.
#'   Default is \code{"https://www.investing.com"}.
#'
#' @return A data frame containing unique market identifiers with the following columns:
#' \itemize{
#'   \item \code{id}: Market identifier
#'   \item \code{name}: Market name
#'   \item \code{flag}: Country flag or code
#'   \item \code{link}: URL path to the market page
#'   \item \code{key}: A unique composite key combining id, name, flag, and link
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Sends an HTTP GET request to the Investing.com indices page
#'   with custom headers to mimic a real browser.
#'   \item Extracts the embedded JSON data from the \code{__NEXT_DATA__} script tag.
#'   \item Parses the JSON content into an R list.
#'   \item Uses \code{find_key_in_json()} to locate the \code{countryStore} node.
#'   \item Extracts country market data from \code{dropdownCountries$countries}.
#'   \item Combines all entries into a single data frame.
#'   \item Creates a unique key for each row and removes duplicates.
#' }
#'
#' Note:
#' \itemize{
#'   \item This function depends on the structure of the Investing.com webpage.
#'   \item Changes to the website may break the function.
#'   \item Currently, the function targets the Nigeria indices page.
#' }
#'
#' @examples
#' \dontrun{
#' markets <- get_all_market_id()
#' head(markets)
#' }
#'
#' @importFrom httr GET add_headers content
#' @importFrom rvest read_html html_element html_text
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate distinct
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{find_key_in_json}} for recursive JSON key extraction
#'
#' @export
get_all_market_id = function(base_url = "https://www.investing.com") {

    params = list(
        `include-major-indices` = "true",
        `include-additional-indices` = "true",
        `include-primary-sectors` = "true",
        `include-other-indices` = "true"
    )

    session = active_client_session(verb = "GET",url = paste0(base_url, "/indices/nigeria-indices"),base_url = base_url)

    headers = session$formated_headers

    req <- httr::GET(
        url = paste0(base_url, "/indices/nigeria-indices"),
        httr::add_headers(.headers = headers),
        query = params
    )

    res = httr::content(req, as = "text")
    html <- rvest::read_html(res)

    parsed <- html |>
        rvest::html_element("script#__NEXT_DATA__") |>
        rvest::html_text() %>%
        jsonlite::fromJSON()

    all_market = find_key_in_json(parsed, "countryStore")

    table_id = all_market$dropdownCountries$countries %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(key = paste(id, name, flag, link, sep = "_")) %>%
        dplyr::distinct(key, .keep_all = TRUE)

    return(table_id)
}


#' Retrieve Market Link by Market ID
#'
#' This function returns the relative URL link associated with a given
#' market identifier. It relies on \code{get_all_market_id()} to fetch
#' the full table of available markets and then filters it to match
#' the provided \code{market_id}.
#'
#' @param market_id A numeric or character value representing the market ID.
#'   Defaults to \code{20}.
#'
#' @return A character vector containing the link associated with the
#'   specified market ID. If no match is found, an empty result is returned.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Calls \code{get_all_market_id()} to retrieve the full market table.
#'   \item Filters the table to find the row where \code{id == market_id}.
#'   \item Extracts the corresponding \code{link} column.
#' }
#'
#' Note: The function depends on \code{get_all_market_id()}, which itself
#' relies on web scraping. Any changes to the source website structure may
#' affect the output.
#'
#' @examples
#' \dontrun{
#' # Get link for a specific market ID
#' get_link(20)
#'
#' # Store result
#' link <- get_link(20)
#' print(link)
#' }
#'
#' @seealso \code{\link{get_all_market_id}} for retrieving the full market table
#'
get_link = function(market_id = 20){
    market_table = get_all_market_id()

    link = market_table[market_table$id == market_id,"link"]

    return(link)
}



#' Retrieve Index Information from Investing.com
#'
#' @description
#' Scrapes and parses index data from Investing.com for a given market.
#' The function builds the appropriate URL using a market ID, sends an
#' authenticated-looking HTTP GET request with browser-like headers, parses
#' the embedded JSON (`__NEXT_DATA__` script tag) from the returned HTML page,
#' and extracts the assets collection containing index metadata.
#'
#' @param market_id A \code{character} or \code{numeric} string identifying the
#'   market on Investing.com. Defaults to \code{"20"}. This ID is passed to the
#'   internal helper \code{get_link()} to resolve the market-specific URL path.
#' @param base_url A \code{character} string specifying the base URL of the
#'   Investing.com website. Defaults to \code{"https://www.investing.com"}.
#'   The country code is automatically extracted from this URL using a regex
#'   pattern on the subdomain (e.g., \code{"fr"} from
#'   \code{"https://fr.investing.com"}).
#'
#' @return A \code{data.frame} (tibble-compatible) containing index information
#'   extracted from the \code{assetsCollectionStore} JSON key. The returned
#'   object includes:
#'   \describe{
#'     \item{...}{All non-empty columns from the parsed assets collection.
#'       Empty or fully \code{NA} columns are dropped automatically.}
#'     \item{full_url}{The absolute URL of each index, constructed by
#'       prepending \code{base_url} to the relative \code{url} field.}
#'     \item{URL}{The raw relative URL path of the index as found in the JSON.}
#'     \item{Type}{A constant character column set to \code{"Index"} for all
#'       rows.}
#'   }
#'   Returns \code{NULL} invisibly if the \code{assetsCollectionStore} key is
#'   not found in the parsed JSON, with a diagnostic \code{message()} emitted.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts a country code from \code{base_url} via regex
#'         (\code{(?<=https://)[a-z]+}).
#'   \item Resolves the market-specific equities link using the internal
#'         helper \code{get_link(market_id)}.
#'   \item Builds the index listing URL in the form:
#'         \code{<base_url>/indices/<country_url_prefix>-indices}.
#'   \item Sends an HTTP GET request with browser-like headers (Chrome 147,
#'         Windows) and query parameters to include major, additional,
#'         primary-sector, and other indices.
#'   \item Retries the request up to 5 times with exponential back-off
#'         (base: 1s, cap: 5s), aborting immediately on HTTP 400, 401, 403,
#'         or 404.
#'   \item Aborts with an error if the final response status is not 200.
#'   \item Parses the HTML response and extracts the embedded
#'         \code{<script id="__NEXT_DATA__">} JSON block (Next.js data island).
#'   \item Searches the parsed JSON tree for the \code{assetsCollectionStore}
#'         key using the internal helper \code{find_key_in_json()}.
#'   \item Binds the collection rows into a single \code{data.frame}, removes
#'         empty columns and rows, and adds derived columns.
#' }
#'
#' @section HTTP Headers:
#' The request mimics a real browser session by setting the following headers:
#' \code{Accept}, \code{Accept-Language}, \code{Priority}, \code{Referer},
#' \code{Sec-CH-UA}, \code{Sec-Fetch-*}, \code{Upgrade-Insecure-Requests}, and
#' \code{User-Agent}. These are required to avoid bot-detection by the server.
#'
#' @section Error Handling:
#' \itemize{
#'   \item If the HTTP response code is not \code{200}, the function calls
#'         \code{rlang::abort("Connection to server failed !")} and stops
#'         execution.
#'   \item If \code{assetsCollectionStore} is not found in the JSON payload,
#'         \code{NULL} is returned with a \code{message()} warning the caller.
#' }
#'
#' @section Dependencies:
#' This function relies on the following internal (non-exported) helpers that
#' must be defined in the same package:
#' \itemize{
#'   \item \code{get_link(market_id)}: resolves the relative equities URL for a
#'         given market ID.
#'   \item \code{find_key_in_json(parsed, key)}: recursively searches a nested
#'         list for a named key and returns its value.
#' }
#'
#' @importFrom stringr str_extract
#' @importFrom httr RETRY add_headers content
#' @importFrom rvest read_html html_element html_text
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows select filter mutate if_any everything where
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' # Fetch index info for the default market (ID = "20")
#' df <- get_index_info_from_investing()
#' head(df)
#'
#' # Fetch index info for a specific market using the French Investing.com site
#' df_fr <- get_index_info_from_investing(
#'   market_id = "20",
#'   base_url   = "https://fr.investing.com"
#' )
#'
#' # Inspect available index URLs
#' df$full_url
#' }
#'
#' @seealso
#' \code{\link[httr]{RETRY}}, \code{\link[rvest]{read_html}},
#' \code{\link[jsonlite]{fromJSON}}, \code{\link[dplyr]{bind_rows}}
#'
get_index_info_from_investing = function(market_id = "20", base_url = "https://www.investing.com") {

    link = get_link(market_id)

    country_url_prefix = gsub("/equities/","",link)

    index_url = paste0(
        base_url,
        "/indices/",
        country_url_prefix,
        "-indices"
    )

    session = active_client_session(verb = "GET",url = index_url,base_url = base_url)
    headers = session$formated_headers
    cookies = session$formated_cookies

    params = list(
        `include-major-indices` = "true",
        `include-additional-indices` = "true",
        `include-primary-sectors` = "true",
        `include-other-indices` = "true"
    )

    req <- httr::RETRY(
        verb = "GET",
        url = paste0(base_url,"/indices/",country_url_prefix,"-indices"),
        httr::add_headers(.headers = headers),
        httr::set_cookies(.cookies = cookies),
        query = params,
        session$user_agent,
        times = 5,
        pause_base = 1,
        pause_cap = 5,
        terminate_on = c(400, 401, 403, 404)
    )

    if(req$status_code != 200) {
        rlang::abort("Connection to server failed !")
    }

    res = httr::content(req, as = "text", encoding = "UTF8")
    html <- rvest::read_html(res)

    parsed <- html |>
        rvest::html_element("script#__NEXT_DATA__") |>
        rvest::html_text() %>%
        jsonlite::fromJSON()

    assets_collection <- find_key_in_json(parsed, "assetsCollectionStore")

    if (!is.null(assets_collection)) {
        df_clean <- assets_collection$assetsCollection$`_collection` %>%
            dplyr::bind_rows() %>%
            dplyr::select(where(~ !all(is.na(.) | . == ""))) %>%
            dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.) & . != "")) %>%
            dplyr::mutate(full_url = paste0(base_url, url)) %>%
            dplyr::mutate(URL = url) %>%
            dplyr::mutate(Type = "Index")
    } else {
        message("Key not found in JSON.")
        return(NULL)
    }

    return(df_clean)
}




#' Retrieve Share (Equity) Information from Investing.com API
#'
#' @description
#' Fetches equity/share data for a given market from the Investing.com
#' internal financial API endpoint. The function sends a browser-mimicking
#' HTTP GET request to \code{api.investing.com/api/financialdata/assets/equitiesByCountry/default},
#' requesting up to 1000 equities with a rich set of fundamental, technical,
#' and performance fields. The JSON response is parsed and returned as a
#' \code{data.frame} enriched with absolute URLs and an asset type label.
#'
#' @param market_id A \code{character} or \code{numeric} string representing
#'   the country/market identifier on Investing.com. Defaults to \code{"20"}.
#'   This value is passed directly as the \code{country-id} query parameter
#'   to the API endpoint.
#' @param base_url A \code{character} string specifying the base URL of the
#'   Investing.com website. Defaults to \code{"https://www.investing.com"}.
#'   Used to:
#'   \itemize{
#'     \item Extract the country code (e.g., \code{"www"} or \code{"fr"})
#'           via regex for the \code{domain-id} request header.
#'     \item Set the \code{Origin} and \code{Referer} headers.
#'     \item Construct the \code{full_url} column in the output.
#'   }
#'
#' @return A \code{data.frame} containing one row per equity listed in the
#'   market, with the following columns sourced from the API response plus
#'   three derived columns:
#'   \describe{
#'     \item{id}{Internal Investing.com asset identifier.}
#'     \item{name}{Full name of the equity.}
#'     \item{symbol}{Ticker symbol of the equity.}
#'     \item{isCFD}{Logical flag indicating whether the instrument is a
#'           Contract for Difference.}
#'     \item{high}{Intraday high price.}
#'     \item{low}{Intraday low price.}
#'     \item{last}{Last traded price.}
#'     \item{lastPairDecimal}{Number of decimal places for the price.}
#'     \item{change}{Absolute price change.}
#'     \item{changePercent}{Percentage price change.}
#'     \item{volume}{Trading volume.}
#'     \item{time}{Timestamp of the last price update.}
#'     \item{isOpen}{Logical flag indicating whether the market is currently
#'           open.}
#'     \item{url}{Relative URL path to the equity page on Investing.com.}
#'     \item{flag}{Country flag code associated with the equity.}
#'     \item{countryNameTranslated}{Translated country name for the equity's
#'           listing.}
#'     \item{exchangeId}{Identifier of the exchange on which the equity is
#'           listed.}
#'     \item{performanceDay}{Daily performance metric.}
#'     \item{performanceWeek}{Weekly performance metric.}
#'     \item{performanceMonth}{Monthly performance metric.}
#'     \item{performanceYtd}{Year-to-date performance metric.}
#'     \item{performanceYear}{1-year performance metric.}
#'     \item{performance3Year}{3-year performance metric.}
#'     \item{technicalHour}{Hourly technical analysis signal.}
#'     \item{technicalDay}{Daily technical analysis signal.}
#'     \item{technicalWeek}{Weekly technical analysis signal.}
#'     \item{technicalMonth}{Monthly technical analysis signal.}
#'     \item{avgVolume}{Average trading volume.}
#'     \item{fundamentalMarketCap}{Market capitalisation.}
#'     \item{fundamentalRevenue}{Annual revenue.}
#'     \item{fundamentalRatio}{Price-to-earnings or other fundamental ratio.}
#'     \item{fundamentalBeta}{Beta coefficient (market sensitivity).}
#'     \item{pairType}{Type classification of the trading pair.}
#'     \item{full_url}{Absolute URL to the equity page, constructed as
#'           \code{paste0(base_url, Url)}.}
#'     \item{URL}{Copy of the raw relative \code{Url} field from the API
#'           response.}
#'     \item{Type}{Constant character column set to \code{"Share"} for all
#'           rows.}
#'   }
#'   Returns \code{NULL} if the HTTP response status is not \code{200}.
#'
#' @details
#' The function proceeds through the following steps:
#' \enumerate{
#'   \item Extracts the country code from \code{base_url} using the regex
#'         pattern \code{(?<=https://)[a-z]+} (e.g., \code{"fr"} from
#'         \code{"https://fr.investing.com"}).
#'   \item Builds browser-like HTTP headers including \code{domain-id},
#'         \code{Origin}, \code{Referer}, CORS-related \code{Sec-Fetch-*}
#'         headers, and a Chrome 147 \code{User-Agent} string.
#'   \item Constructs query parameters requesting up to \strong{1000 equities}
#'         (\code{page-size = 1000}) with \strong{31 data fields} covering
#'         prices, volumes, performance windows, technical signals, and
#'         fundamental ratios.
#'   \item Sends a GET request to the Investing.com financial API with up to
#'         \strong{5 retries} (exponential back-off: base 1s, cap 5s),
#'         terminating immediately on HTTP 400, 401, 403, or 404.
#'   \item Aborts with \code{rlang::abort()} and returns \code{NULL} if the
#'         final status code is not \code{200}.
#'   \item Parses the JSON response body (UTF-8) and extracts the \code{data}
#'         element.
#'   \item Mutates the result to add \code{full_url}, \code{URL}, and
#'         \code{Type} columns.
#' }
#'
#' @section API Endpoint:
#' This function targets the private/internal Investing.com REST API:
#' \preformatted{
#' GET https://api.investing.com/api/financialdata/assets/equitiesByCountry/default
#' }
#' This endpoint is not publicly documented and may change without notice.
#' The request mimics a CORS \code{same-site} browser call by setting
#' \code{Sec-Fetch-Mode: cors} and \code{Sec-Fetch-Site: same-site}.
#'
#' @section HTTP Headers:
#' The following headers are set to bypass bot detection:
#' \itemize{
#'   \item \code{Accept: */*}
#'   \item \code{Accept-Language}: English (US) with French fallback.
#'   \item \code{domain-id}: Country code extracted from \code{base_url}.
#'   \item \code{Origin} / \code{Referer}: set to \code{base_url}.
#'   \item \code{Sec-CH-UA}: Chrome 147 brand token.
#'   \item \code{Sec-Fetch-Dest/Mode/Site}: CORS browser fetch profile.
#'   \item \code{User-Agent}: Windows Chrome 147 string.
#' }
#'
#' @section Error Handling:
#' \itemize{
#'   \item Non-200 HTTP responses trigger \code{rlang::abort()} with the
#'         message \code{"Index extraction : Connection to server failed!"}
#'         and return \code{NULL}.
#'   \item Transient errors (5xx, timeouts) are retried automatically up to
#'         5 times via \code{httr::RETRY()}.
#'   \item HTTP 400, 401, 403, and 404 responses cause immediate termination
#'         without retrying.
#' }
#'
#' @importFrom stringr str_extract
#' @importFrom httr RETRY add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Fetch equities for the default market (ID = "20")
#' shares <- get_share_info_from_investing()
#' head(shares)
#'
#' # Fetch equities from the French Investing.com site for market ID "20"
#' shares_fr <- get_share_info_from_investing(
#'   market_id = "20",
#'   base_url  = "https://fr.investing.com"
#' )
#'
#' # Check available columns
#' colnames(shares_fr)
#'
#' # Filter only open markets with market cap available
#' library(dplyr)
#' open_shares <- shares_fr |>
#'   dplyr::filter(isOpen == TRUE, !is.na(fundamentalMarketCap)) |>
#'   dplyr::arrange(dplyr::desc(fundamentalMarketCap))
#' }
#'
#' @seealso
#' \code{\link{get_index_info_from_investing}} for retrieving index data,
#' \code{\link[httr]{RETRY}} for the retry mechanism,
#' \code{\link[jsonlite]{fromJSON}} for JSON parsing.
#'
get_share_info_from_investing = function(market_id = "20", base_url = "https://www.investing.com") {

    share_url = "https://api.investing.com/api/financialdata/assets/equitiesByCountry/default"

    session = active_client_session(verb = "GET",url = "https://www.investing.com",base_url = base_url)
    headers = session$formated_headers
    cookies = session$formated_cookies

    params = list(
        `fields-list` = "id,name,symbol,isCFD,high,low,last,lastPairDecimal,change,changePercent,volume,time,isOpen,url,flag,countryNameTranslated,exchangeId,performanceDay,performanceWeek,performanceMonth,performanceYtd,performanceYear,performance3Year,technicalHour,technicalDay,technicalWeek,technicalMonth,avgVolume,fundamentalMarketCap,fundamentalRevenue,fundamentalRatio,fundamentalBeta,pairType",
        `country-id` = market_id,
        `filter-domain` = "",
        page = "0",
        `page-size` = "100000",
        limit = "0",
        `include-additional-indices` = "false",
        `include-major-indices` = "false",
        `include-other-indices` = "false",
        `include-primary-sectors` = "false",
        `include-market-overview` = "false"
    )

    req <- httr::RETRY(
        verb = "GET",
        url = share_url,
        httr::add_headers(.headers = headers),
        httr::set_cookies(.cookies = cookies),
        handle = session$handle,
        query = params,
        session$user_agent,
        times = 5,
        pause_base = 1,
        pause_cap = 3,
        quiet = TRUE
    )

    if(req$status_code != 200) {
        rlang::abort("Index extraction : Connection to server failed!")
        return(NULL)
    }

    res = httr::content(req, as = "text", encoding = "UTF8") %>%
        jsonlite::fromJSON()

    share_info = res$data %>%
        dplyr::mutate(full_url = paste0(base_url, Url)) %>%
        dplyr::mutate(URL = Url) %>%
        dplyr::mutate(Type = "Share")

    return(share_info)
}








#' Retrieve Combined Index and Share Data from Investing.com
#'
#' This function retrieves both index and share (equity) data for a given
#' market from Investing.com by combining the outputs of
#' \code{get_index_info_from_investing()} and
#' \code{get_share_info_from_investing()}.
#'
#' @param market_id A character or numeric value representing the market ID.
#'   Default is \code{"20"}.
#'
#' @return A named list with two elements:
#' \itemize{
#'   \item \code{Index}: A data frame containing index information.
#'   \item \code{Share}: A data frame containing share (equity) information.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Calls \code{get_index_info_from_investing()} to retrieve index data.
#'   \item Calls \code{get_share_info_from_investing()} to retrieve share data.
#'   \item Combines both outputs into a single named list.
#' }
#'
#' This function acts as a wrapper to simplify access to both datasets
#' in a single call.
#'
#' Note: The function depends on web scraping and API calls. Any change
#' in the Investing.com structure or API may affect the results.
#'
#' @examples
#' \dontrun{
#' data <- investing_share_index_info(20)
#'
#' # Access index data
#' head(data$Index)
#'
#' # Access share data
#' head(data$Share)
#' }
#'
#' @seealso \code{\link{get_index_info_from_investing}},
#'   \code{\link{get_share_info_from_investing}}
#'
investing_share_index_info = function(market_id = "20"){

    df_index = get_index_info_from_investing(market_id = market_id)
    df_share = get_share_info_from_investing(market_id = market_id)

    return(
        list(
            Index = df_index,
            Share = df_share
        )
    )
}






#' Retrieve Simplified Market Index and Share Information
#'
#' This function retrieves both index and share (equity) information for a given
#' market from Investing.com, and returns a simplified and standardized structure.
#'
#' It relies on \code{investing_share_index_info()} to fetch raw data, then
#' selects and renames key variables for easier downstream use.
#'
#' @param market_id A character or numeric value representing the market ID.
#'   Default is \code{"20"}.
#'
#' @return A named list with two data frames:
#' \itemize{
#'   \item \code{Index}: A data frame containing index information with columns:
#'   \itemize{
#'     \item \code{Ticker}: Index symbol
#'     \item \code{Name}: Index name
#'     \item \code{Type}: Asset type (always "Index")
#'   }
#'   \item \code{Share}: A data frame containing share (equity) information with columns:
#'   \itemize{
#'     \item \code{Ticker}: Share symbol
#'     \item \code{Name}: Share name
#'     \item \code{Country.code}: Country flag/code
#'     \item \code{FundamentalMarketCap}: Market capitalization
#'     \item \code{FundamentalBeta}: Beta coefficient
#'     \item \code{Type}: Asset type (always "Share")
#'   }
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Calls \code{investing_share_index_info()} to retrieve raw index and share data.
#'   \item Extracts relevant columns from both datasets.
#'   \item Renames columns to standardized names:
#'   \itemize{
#'     \item \code{symbol} / \code{Symbol} → \code{Ticker}
#'     \item \code{name} / \code{Name} → \code{Name}
#'     \item \code{Flag} → \code{Country.code}
#'   }
#'   \item Returns a structured list containing cleaned datasets.
#' }
#'
#' Note: This function assumes that the structure returned by
#' \code{investing_share_index_info()} contains two elements:
#' \code{Index} and \code{Share}. Any structural change may break the function.
#'
#' @examples
#' \dontrun{
#' data <- get_market_share_index_from_investing(20)
#'
#' # Access index data
#' head(data$Index)
#'
#' # Access share data
#' head(data$Share)
#' }
#'
#' @importFrom dplyr select rename %>%
#'
#' @seealso \code{\link{investing_share_index_info}}
#'
get_market_share_index_from_investing = function(market_id = "20"){
    market_info = investing_share_index_info(market_id = market_id)

    # FOR INDEX
    # c("mobx_easy_id", "id", "name", "symbol", "exchangeId", "flagCode",
    #   "flagName", "title", "url", "decimalPrecision", "isCFD", "last",
    #   "lastPairDecimal", "high", "low", "changeOneDayPercent", "changeOneDay",
    #   "lastUpdateTime", "isActualOpen", "Type")
    # >

    # FOR SHARE
    # c("AvgVolume", "Chg", "ChgPct", "CountryNameTranslated", "ExchangeId",
    #   "Flag", "FundamentalBeta", "FundamentalMarketCap", "FundamentalRatio",
    #   "FundamentalRevenue", "High", "Id", "IsCFD", "IsOpen", "Last",
    #   "LastPairDecimal", "Low", "Name", "PairType", "Performance3Year",
    #   "PerformanceDay", "PerformanceMonth", "PerformanceWeek", "PerformanceYear",
    #   "PerformanceYtd", "Symbol", "TechnicalDay", "TechnicalHour",
    #   "TechnicalMonth", "TechnicalWeek", "Time", "Url", "Volume", "Type"
    # )

    return(
        list(
            Index = market_info$Index %>% select(symbol,name,Type) %>% rename(Ticker = symbol,Name = name) %>% arrange(Ticker),
            Share = market_info$Share %>% select(Symbol,Name,
                                                 # Sector,NatureofBusiness,
                                                 Flag,FundamentalMarketCap,FundamentalBeta,Type) %>% rename(Ticker = Symbol,Country.code = Flag) %>% arrange(Ticker)
        )
    )
}



#' Search Instrument Information from Investing.com API
#'
#' This function queries the Investing.com search API to retrieve information
#' about a financial instrument (e.g., stock, index, currency) based on a ticker.
#' It uses an active HTTP session to send the request with appropriate headers
#' and cookies.
#'
#' @param ticker A character string representing the instrument symbol or keyword
#'   to search for.
#' @param base_url A character string representing the base URL used to initialize
#'   or retrieve the active session (e.g., \code{"https://www.investing.com"}).
#'
#' @return A list containing information about the first matching instrument,
#'   or \code{NULL} if no match is found.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Initializes or retrieves an active session using
#'   \code{active_client_session()}.
#'   \item Sends an HTTP request to the Investing.com search API endpoint.
#'   \item Includes session headers and cookies to mimic a browser request.
#'   \item Parses the JSON response.
#'   \item Extracts the \code{quotes} field.
#'   \item Returns the first matching result if available.
#' }
#'
#' Note:
#' \itemize{
#'   \item This function relies on an undocumented API endpoint.
#'   \item The use of the HTTP \code{OPTIONS} method is non-standard for data retrieval
#'   and may stop working if the API changes.
#' }
#'
#' @examples
#' \dontrun{
#' info <- search_instrument_info("AAPL", "https://www.investing.com")
#' print(info)
#' }
#'
#' @importFrom httr VERB add_headers set_cookies content
#'
#' @seealso \code{\link{active_client_session}}
#'
#' @export
search_instrument_info = function(ticker, base_url) {

    params = list(q = as.character(ticker))

    session = active_client_session(verb = "GET",url = base_url)

    req <- httr::VERB(
        "OPTIONS",
        url = "https://api.investing.com/api/search/v2/search",
        httr::add_headers(.headers = session$headers),
        httr::set_cookies(.cookies = session$cookies),
        query = params
    )

    if (req$status_code != 200) {
        rlang::abort("Search request failed")
    }

    res = httr::content(req, as = "parsed")

    if (is.null(res$quotes) || length(res$quotes) == 0L) {
        return(NULL)
    }

    return(res$quotes[[1]])
}





#' Retrieve Unified Instrument Information (Index + Shares)
#'
#' This function retrieves and consolidates financial instrument metadata
#' (both indices and shares) for a given market ID. It standardizes and merges
#' data from \code{investing_share_index_info()} into a single structured table.
#'
#' @param market_id A character or numeric value representing the market ID
#'   used to query Investing.com data sources.
#'
#' @return A data frame containing combined instrument information with the
#' following standardized columns:
#' \itemize{
#'   \item \code{SYMBOL}: Instrument symbol (index or share ticker)
#'   \item \code{ID}: Unique instrument identifier
#'   \item \code{URL}: Relative URL of the instrument page
#'   \item \code{Type}: Asset type ("Index" or "Share")
#' }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Calls \code{investing_share_index_info()} to retrieve both index
#'   and share datasets.
#'   \item Extracts relevant fields from each dataset:
#'   \itemize{
#'     \item Index: \code{symbol}, \code{id}, \code{URL}, \code{Type}
#'     \item Share: \code{Symbol}, \code{Id}, \code{URL}, \code{Type}
#'   }
#'   \item Renames columns to a unified schema (\code{SYMBOL}, \code{ID}).
#'   \item Merges both datasets using \code{rbind()}.
#'   \item Sorts the final output by \code{SYMBOL}.
#' }
#'
#' Note: This function assumes that both Index and Share datasets contain
#' consistent column names. Any structural change in upstream functions may
#' cause errors.
#'
#' @examples
#' \dontrun{
#' instruments <- .get_instrument_info(20)
#' head(instruments)
#'
#' # Filter specific instrument type
#' subset(instruments, Type == "Share")
#' }
#'
#' @importFrom dplyr select rename arrange
#'
#' @seealso \code{\link{investing_share_index_info}}
#'
.get_instrument_info = function(market_id) {

    market_info = investing_share_index_info(market_id = market_id)

    symbol_id = rbind(
        market_info$Index %>%
            dplyr::select(symbol, id, URL, Type) %>%
            dplyr::rename(SYMBOL = symbol, ID = id),

        market_info$Share %>%
            dplyr::select(Symbol, Id, URL, Type) %>%
            dplyr::rename(SYMBOL = Symbol, ID = Id)
    ) %>%
        dplyr::arrange(SYMBOL)

    return(symbol_id)
}




#' Populate a Market Object with Tickers from Investing.com
#'
#' This internal function retrieves and formats ticker data (indexes and shares)
#' from Investing.com and populates a provided S4 market object. It standardizes
#' the structure of market data by assigning indexes, shares, and ticker lists
#' to the appropriate slots of the market object.
#'
#' @param MARKET_OBJECT An S4 market object (typically created using
#'   \code{CREATE_ALL_MARKETS()}) that will be populated with ticker data.
#' @param market_id A character or numeric value representing the market ID
#'   used by Investing.com. Default is \code{"20"}.
#'
#' @return The updated S4 market object with the following slots populated:
#' \itemize{
#'   \item \code{Indexes}: Data frame of index tickers (Ticker, Name)
#'   \item \code{Shares}: Data frame of share tickers (Ticker and attributes)
#'   \item \code{ListIndexes}: Character vector of index tickers
#'   \item \code{ListShares}: Character vector of share tickers
#'   \item \code{List}: Combined vector of all tickers (indexes + shares)
#'   \item \code{Ticker_full_name}: Alias of \code{List}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Retrieves raw ticker data using
#'         \code{get_market_share_index_from_investing()}.
#'   \item Filters index data and assigns it to the \code{Indexes} slot.
#'   \item Filters share data and assigns it to the \code{Shares} slot.
#'   \item Builds ticker name vectors for indexes and shares.
#'   \item Combines all tickers into a unified list.
#' }
#'
#' Error and warning handling is implemented using \code{tryCatch()}.
#' In case of failure (e.g., network issues or API changes), a message is displayed
#' and the function returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' market_obj <- CREATE_ALL_MARKETS()$NGX_MARKET
#'
#' market_obj <- .GET_TICKER_FROM_INVESTING(
#'   MARKET_OBJECT = market_obj,
#'   market_id = "20"
#' )
#'
#' head(market_obj@Indexes)
#' head(market_obj@Shares)
#' market_obj@List
#' }
#'
#' @seealso \code{\link{get_market_share_index_from_investing}},
#'   \code{\link{CREATE_ALL_MARKETS}}
#'
#' @keywords internal
.GET_TICKER_FROM_INVESTING = function(MARKET_OBJECT,market_id = "20") {

    ticker_data = get_market_share_index_from_investing(market_id = market_id)

    # MISE EN FORMAT SELON L'AFFICHAGE STANDARD

    # Indexes
    MARKET_OBJECT@Indexes = ticker_data$Index[ticker_data$Index$Type == "Index",1:2]
    rownames(MARKET_OBJECT@Indexes) = NULL

    # Shares
    MARKET_OBJECT@Shares = ticker_data$Share[ticker_data$Share$Type == "Share",c(1:5)]
    rownames(MARKET_OBJECT@Shares) = NULL

    # Name List
    MARKET_OBJECT@ListIndexes = MARKET_OBJECT@Indexes[,1]
    MARKET_OBJECT@ListShares = MARKET_OBJECT@Shares[,1]
    MARKET_OBJECT@List = c(MARKET_OBJECT@ListIndexes,MARKET_OBJECT@ListShares)
    MARKET_OBJECT@Ticker_full_name = MARKET_OBJECT@List

    return(MARKET_OBJECT)
}



#' Retrieve Historical Market Data from Investing.com API
#'
#' @description
#' Internal helper function used to retrieve historical OHLCV
#' (Open, High, Low, Close, Volume) market data for one or multiple
#' financial instruments from the Investing.com API.
#'
#' The function supports flexible ticker selection, customizable
#' date ranges, automatic request chunking, retry handling,
#' and multiple output structures for downstream analysis.
#'
#' @param market_code A character string representing the market code
#'   (e.g. \code{"NGX"}, \code{"JSE"}, \code{"EGX"}).
#'   Used to retrieve available market tickers.
#'
#' @param market_id A character or numeric identifier corresponding
#'   to the Investing.com market identifier.
#'
#' @param ticker A character vector containing ticker symbols
#'   to retrieve (case-insensitive).
#'
#'   Special values include:
#'   \itemize{
#'     \item \code{"ALL"}: retrieve all available instruments
#'     \item \code{"ALL SHARES"}: retrieve all listed shares only
#'     \item \code{"ALL INDEXES"}: retrieve all market indices only
#'   }
#'
#' @param Period Data frequency. Currently only
#'   \code{"daily"} is supported.
#'
#' @param from Start date of the extraction window.
#'   Can be either a \code{Date} object or a character string
#'   formatted as \code{"YYYY-MM-DD"}.
#'
#' @param to End date of the extraction window.
#'   Can be either a \code{Date} object or a character string
#'   formatted as \code{"YYYY-MM-DD"}.
#'
#' @param output_format Output structure specification:
#'   \itemize{
#'     \item \code{"all"}: returns both formats as a named list (default)
#'     \item \code{"by_col"}: returns a long-format data frame
#'     \item \code{"by_row"}: returns a wide-format data frame
#'   }
#'
#' @param base_url Base Investing.com regional URL used to initialize
#'   the HTTP session and cookies.
#'
#' @details
#' The function performs the following operations:
#'
#' \enumerate{
#'   \item Retrieves available market tickers using \code{GET_tickers()}.
#'   \item Maps symbols to Investing.com instrument identifiers using
#'   \code{.get_instrument_info()}.
#'   \item Validates user-provided tickers and date ranges.
#'   \item Splits large requests into chunks of approximately 2000 days.
#'   \item Initializes authenticated browser-like sessions.
#'   \item Sends requests to the Investing.com historical API endpoint.
#'   \item Cleans and standardizes OHLCV variables.
#'   \item Aggregates data across periods and tickers.
#'   \item Formats outputs using \code{output_data()}.
#' }
#'
#' To improve reliability, the function uses
#' \code{httr::RETRY()} with automatic retry handling.
#'
#' Random delays are inserted between requests to reduce
#' throttling risks and server load.
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
#'   \item A wide-format data frame (\code{"by_row"}) where variables
#'   are prefixed with ticker names
#'   (e.g. \code{ZENITHBANK.Close}).
#'
#'   \item A named list containing both formats when
#'   \code{output_format = "all"}.
#' }
#'
#' @section Request Chunking:
#' Large extraction periods are automatically split into intervals
#' of approximately 2000 days in order to comply with
#' Investing.com API limitations.
#'
#' @section Retry Strategy:
#' HTTP requests are performed using \code{httr::RETRY()}
#' with configurable retry attempts and timeout handling.
#'
#' @section Error Handling:
#' The function:
#'
#' \itemize{
#'   \item stops if \code{from >= to},
#'   \item stops if \code{from > Sys.Date()},
#'   \item skips unavailable or invalid tickers,
#'   \item displays informative progress and error messages.
#' }
#'
#' @section Caching:
#' The most recent successful extraction is stored in
#' \code{.pkg_env$last_data_downloaded} for reuse during
#' the current R session.
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @seealso
#' \code{\link{GET_data}},
#' \code{\link{GET_tickers}},
#' \code{\link{output_data}},
#' \code{\link{clean_numeric}}
#'
#' @importFrom httr RETRY add_headers content status_code timeout
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct arrange mutate
#' @importFrom dplyr select rename across
#' @importFrom stats runif
#' @importFrom rlang abort
#'
#' @keywords internal
.GET_DATA_FROM_INVESTING = function(market_code = "NGX",market_id = "20",ticker = "ALL",Period = "daily",from = Sys.Date() - 89,to = Sys.Date(),output_format = c("all","by_col","by_row"),base_url = "https://www.investing.com") {

    # ====================================================
    max_retries = 5
    retry_delay = 3

    # ==================================================== Tickers
    market_tickers = GET_tickers(toupper(market_code))
    market_ticker_info = .get_instrument_info(market_id =  market_id)
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

    # t_range = as.character(to - from + 1)

    periods = seq(from = from, to = to, by = "2000 day")
    if(!(to %in% periods)){
        periods <- c(periods,to)
    }

    # ==================================================== DATA BETWEEN PERIODS


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

            from_date <- as.Date(periods[i])
            to_date <- as.Date(periods[i+1])

            params = list(
                `start-date` = from_date,
                `end-date` = to_date,
                `time-frame` = "Daily",
                `add-missing-rows` = "false"
            )

            session = active_client_session(verb = "GET",url = "https://www.investing.com",base_url = base_url)
            tick_info = head(market_ticker_info[which(market_ticker_info$SYMBOL == tick),],1)

            if(is.null(tick_info)){
                next
            }

            histo_url = paste0(tick_info$URL,"-historical-data")
            histo_url_basename = basename(histo_url)

            headers = session$formated_headers
            cookies = session$formated_cookies

            params = list(
                `start-date` = from_date,
                `end-date` = to_date,
                `time-frame` = "Daily",
                `add-missing-rows` = "false"
            )

            req <- tryCatch({

                httr::RETRY(
                    verb = "GET",
                    url = paste0("https://api.investing.com/api/financialdata/historical/", tick_info$ID),
                    httr::add_headers(.headers = headers),
                    httr::set_cookies(.cookies = cookies),
                    session$user_agent,
                    handle = session$handle,
                    query = params,
                    pause_base = retry_delay,
                    times = max_retries,
                    quiet = TRUE
                )

            }, error = function(e) NULL)

            Sys.sleep(runif(1,0.1,0.7))

            if (inherits(req, "try-error") || status_code(req) != 200) {
                message(paste("Can't extract data for",tick))
                next
            }

            txt = req %>%
                content(as = "text",encoding = "UTF8") %>%
                fromJSON()

            df_test <- tryCatch(
                txt$data %>%
                    as.data.frame() %>%
                    dplyr::transmute(
                        Date   = as.Date(rowDateTimestamp),
                        Open   = as.numeric(last_open),
                        High   = as.numeric(last_max),
                        Low    = as.numeric(last_min),
                        Close  = as.numeric(last_close),
                        Volume = as.numeric(volumeRaw)
                    ) %>%
                    dplyr::arrange(Date),
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


