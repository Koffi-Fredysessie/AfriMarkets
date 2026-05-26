#' Recursively Find a Value by Key in a Nested JSON/List Structure
#'
#' This function searches for a specified key within a nested list or JSON-like
#' structure and returns the first matching value found. The search is performed
#' recursively, exploring all levels of the structure until the key is located.
#'
#' If multiple occurrences of the key exist, only the first encountered value
#' (in depth-first order) is returned. If the key is not found, the function
#' returns \code{NULL}.
#'
#' @param x A list or JSON-like object (typically obtained from parsing JSON)
#'   in which to search for the key.
#' @param key A character string representing the name of the key to search for.
#'
#' @return The value associated with the first occurrence of the specified key,
#'   or \code{NULL} if the key is not found.
#'
#' @details
#' The function works recursively:
#' \itemize{
#'   \item If \code{x} is a list and contains the key, the corresponding value is returned.
#'   \item Otherwise, the function applies itself to each element of the list.
#'   \item The first non-\code{NULL} result is returned.
#' }
#'
#' This function is particularly useful for navigating deeply nested JSON
#' structures where the exact location of a key is unknown.
#'
#' @examples
#' \dontrun{
#' library(purrr)
#'
#' nested_list <- list(
#'   a = list(
#'     b = list(
#'       target = 42
#'     )
#'   ),
#'   c = list(
#'     d = 100
#'   )
#' )
#'
#' find_key_in_json(nested_list, "target")
#' # Returns 42
#'
#' find_key_in_json(nested_list, "d")
#' # Returns 100
#'
#' find_key_in_json(nested_list, "not_here")
#' # Returns NULL
#' }
#'
#' @importFrom purrr map compact
#'
find_key_in_json <- function(x, key) {
    if (is.list(x)) {
        if (key %in% names(x)) {
            return(x[[key]])
        } else {
            result <- map(x, ~ find_key_in_json(.x, key))
            result <- compact(result)
            if (length(result) > 0) {
                return(result[[1]])
            }
        }
    }
    return(NULL)
}




#' Create an HTTP Client Session with Persistent Headers and Cookies
#'
#' This function initializes an HTTP session using \code{httr}, retrieves
#' response headers and cookies, and stores session metadata in a local
#' environment for reuse. It is designed to mimic a browser session when
#' interacting with web APIs or scraping dynamic websites.
#'
#' @param url A character string specifying the target URL to initiate the session.
#' @param base_url A character string specifying the base URL used to create the
#'   HTTP handle. Defaults to \code{url} if not provided.
#' @param original_headers A named character vector of HTTP headers to include
#'   in the request. If \code{NULL}, a default browser-like header set is used.
#'
#' @return A list representing the session with the following elements:
#' \itemize{
#'   \item \code{basic_req}: The original \code{httr} response object
#'   \item \code{headers}: Flattened string of all response headers
#'   \item \code{mini_headers}: Simplified headers from the response
#'   \item \code{cookies}: Cookie string extracted from the response
#'   \item \code{handle}: The \code{httr} handle used for persistent sessions
#'   \item \code{buildID}: Version identifier extracted from headers (if available)
#'   \item \code{original_headers}: Headers used to create the request
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Creates an HTTP handle using \code{httr::handle()}.
#'   \item Sends a GET request with custom headers.
#'   \item Validates the response status code.
#'   \item Extracts headers and cookies from the response.
#'   \item Stores session metadata in \code{.pkg_env}.
#' }
#'
#' This function is useful for maintaining authenticated or stateful
#' interactions with web services.
#'
#' @examples
#' \dontrun{
#' session <- create_client_session("https://www.investing.com")
#' session$cookies
#' }
#'
#' @importFrom httr GET handle add_headers
#' @importFrom rlang abort
#'
create_client_session <- function(url, base_url = NULL, original_headers = NULL) {

    if(is.null(base_url)) base_url = url

    if(is.null(original_headers)) {
        test_url = httr::HEAD(base_url)

        if(test_url$status_code != 200) {
            rlang::abort("Connection to server failed!")
        } else {
            headers = test_url$all_headers[[1]]
            cookies = test_url$cookies
            .pkg_env$`start-time-session` = Sys.time()
        }
    }

    h = httr::handle(base_url)

    session = list(
        headers = headers,
        cookies = cookies,
        handle = h,
        buildID = find_key_in_json(test_url$all_headers,"inv-version"),
        original_headers = original_headers
    )

    .pkg_env$`current-session` = session

    return(session)
}


#' Retrieve or Refresh an Active HTTP Client Session
#'
#' This function returns an active HTTP session stored in a local environment.
#' If the current session has expired or is invalid, a new session is created
#' using \code{create_client_session()}.
#'
#' @param url A character string specifying the target URL.
#' @param base_url A character string specifying the base URL for the session.
#' @param session A session object previously created by
#'   \code{create_client_session()}. Defaults to the current session stored in
#'   \code{.pkg_env}.
#'
#' @return A valid session object (list) containing request, headers, cookies,
#'   and handle information.
#'
#' @details
#' The function performs the following checks:
#' \enumerate{
#'   \item Verifies if the session exists and has not expired.
#'   \item If expired or missing, creates a new session.
#'   \item Ensures the URL ends with a trailing slash.
#'   \item Compares the requested URL with the session URL.
#'   \item Refreshes the session if the URL has changed.
#' }
#'
#' Session lifetime is controlled via \code{.pkg_env$life-time-session}.
#'
#' @examples
#' \dontrun{
#' session <- active_client_session("https://www.investing.com")
#' session$cookies
#' }
#'
#' @seealso \code{\link{create_client_session}}
#'
active_client_session = function(url, base_url = NULL, session = .pkg_env$`current-session`) {

    if ((Sys.time() - .pkg_env$`start-time-session` > .pkg_env$`life-time-session`) || is.null(session)) {
        session = create_client_session(url = url, base_url = base_url)
    }

    # Ensure trailing slash
    if (substr(url, nchar(url), nchar(url)) != "/") {
        url = paste0(url, "/")
    }

    # Refresh session if URL changed
    if (session$basic_req$url != url) {
        session = create_client_session(url = url, base_url = base_url)
    }

    return(session)
}
