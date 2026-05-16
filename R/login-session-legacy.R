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
        original_headers = c(
            accept = "*/*",
            `accept-language` = "en-US,en;q=0.9,fr-FR;q=0.8,fr;q=0.7",
            origin = base_url,
            priority = "u=1, i",
            referer = base_url,
            `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36"
        )
    }

    h = httr::handle(base_url)
    req <- httr::GET(url = url, httr::add_headers(.headers = original_headers), handle = h)

    if(req$status_code == 200) {
        .pkg_env$`start-time-session` = Sys.time()
    } else {
        rlang::abort("Connection to server failed!")
    }

    headers = paste(paste0(names(req$all_headers), "=", unlist(req$all_headers)), collapse = "; ")
    headers2 = paste(paste0(names(req$headers), "=", unlist(req$headers)), collapse = "; ")
    cookies = paste(paste0(req$cookies$name, "=", req$cookies$value), collapse = "; ")

    session = list(
        basic_req = req,
        headers = headers,
        mini_headers = headers2,
        cookies = cookies,
        handle = h,
        buildID = req$headers$`inv-version`,
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
