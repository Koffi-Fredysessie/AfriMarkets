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
#' @description
#' Initialises a stateful HTTP session by sending a probe request to
#' \code{base_url}, extracting the server's response headers and cookies,
#' then performing a fully authenticated retry request to \code{url}.
#' The resulting session object is stored in the package-level environment
#' \code{.pkg_env} for reuse by \code{\link{active_client_session}}.
#' The request mimics a real browser user-agent built from the local
#' system information.
#'
#' @param verb A \code{character} string specifying the HTTP method to use
#'   (e.g., \code{"GET"}, \code{"POST"}). Passed directly to
#'   \code{httr::VERB()} and \code{httr::RETRY()}.
#' @param url A \code{character} string giving the target URL for the
#'   authenticated session request (may differ from \code{base_url}).
#' @param base_url A \code{character} string giving the base URL used for
#'   the probe request and handle creation. Defaults to \code{url} when
#'   \code{NULL}.
#' @param query A named \code{list} of query parameters appended to the
#'   request URL. Defaults to \code{NULL} (no query string).
#' @param original_headers A named \code{character} vector of custom HTTP
#'   headers. When \code{NULL} (default), headers are automatically derived
#'   from the probe response; otherwise the provided headers are used
#'   directly.
#' @param pause A \code{numeric} scalar (seconds) setting the cap for the
#'   exponential back-off between retries. Defaults to \code{3}.
#' @param times A \code{numeric} scalar giving the maximum number of retry
#'   attempts. Defaults to \code{5}.
#'
#' @return A named \code{list} representing the session, with the following
#'   elements:
#'   \describe{
#'     \item{\code{basic_req}}{The final \code{httr} response object from
#'           the authenticated request to \code{url}.}
#'     \item{\code{headers}}{Raw \code{httr} response headers from the
#'           probe request (\code{httr::headers()}).}
#'     \item{\code{formated_headers}}{Named \code{character} vector of
#'           flattened headers ready for use in \code{httr::add_headers()}.}
#'     \item{\code{cookies}}{Raw cookie data frame from the probe response
#'           (\code{httr::cookies()}).}
#'     \item{\code{formated_cookies}}{Named \code{character} vector of
#'           cookies ready for use in \code{httr::set_cookies()}.}
#'     \item{\code{handle}}{The persistent \code{httr} handle bound to
#'           \code{base_url}.}
#'     \item{\code{buildID}}{The \code{"inv-version"} header value extracted
#'           from \code{all_headers} via \code{find_key_in_json()}, or
#'           \code{NULL} if absent.}
#'     \item{\code{original_headers}}{The \code{original_headers} argument
#'           as passed by the caller.}
#'     \item{\code{user_agent}}{The \code{httr} user-agent object built from
#'           local system information.}
#'   }
#'   The session is also stored in \code{.pkg_env$`current-session`} and
#'   the creation timestamp in \code{.pkg_env$`start-time-session`}.
#'
#' @details
#' The function proceeds through the following steps:
#' \enumerate{
#'   \item Builds a \code{User-Agent} string from \code{verb},
#'         \code{"httr"}, the local OS name, and OS version via
#'         \code{Sys.info()}.
#'   \item Falls back \code{base_url <- url} if \code{base_url} is
#'         \code{NULL}.
#'   \item When \code{original_headers} is \code{NULL}:
#'     \enumerate{
#'       \item Sends a probe \code{VERB} request to \code{base_url}.
#'       \item Aborts with \code{rlang::abort()} if the status is not
#'             \code{200}.
#'       \item Extracts response headers; if \code{domain-id} is missing,
#'             injects \code{accept}, \code{access-control-request-headers},
#'             and \code{domain-id} (via the internal helper
#'             \code{extract_subdomain()}).
#'       \item Extracts cookies and creates a persistent \code{httr} handle.
#'       \item Sends a fully authenticated \code{RETRY} request to
#'             \code{url} (up to \code{times} attempts, back-off capped at
#'             \code{pause} seconds).
#'       \item Records \code{Sys.time()} in
#'             \code{.pkg_env$`start-time-session`}.
#'     }
#'   \item Assembles and returns the session list.
#' }
#'
#' @section Package Environment:
#' This function writes to the following keys in \code{.pkg_env}:
#' \itemize{
#'   \item \code{.pkg_env$`current-session`}: the complete session list.
#'   \item \code{.pkg_env$`start-time-session`}: \code{POSIXct} timestamp
#'         of session creation, used by \code{active_client_session()} for
#'         TTL checks.
#' }
#'
#' @section Internal Dependencies:
#' \itemize{
#'   \item \code{extract_subdomain(url)}: extracts the subdomain from a URL
#'         string for the \code{domain-id} header.
#'   \item \code{find_key_in_json(x, key)}: recursively searches response
#'         headers for the \code{"inv-version"} build identifier.
#' }
#'
#' @section Error Handling:
#' \itemize{
#'   \item If the probe request to \code{base_url} returns a non-200 status,
#'         \code{rlang::abort("Connection to server failed!")} is called
#'         immediately.
#'   \item Transient failures during the authenticated request are retried
#'         automatically up to \code{times} times.
#' }
#'
#' @importFrom httr VERB RETRY handle add_headers set_cookies cookies headers user_agent
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' # Basic session on the default Investing.com site
#' session <- create_client_session(
#'   verb     = "GET",
#'   url      = "https://www.investing.com"
#' )
#' session$buildID
#' session$formated_cookies
#'
#' # Session with a distinct base URL and target URL
#' session <- create_client_session(
#'   verb     = "GET",
#'   url      = "https://api.investing.com/api/financialdata/assets",
#'   base_url = "https://www.investing.com",
#'   pause    = 5,
#'   times    = 3
#' )
#' }
#'
#' @seealso
#' \code{\link{active_client_session}},
#' \code{\link[httr]{RETRY}},
#' \code{\link[httr]{handle}}
#'
#' @keywords internal
create_client_session <- function(verb,url,base_url = NULL, query = NULL, original_headers = NULL,pause = 3,times = 5) {

    local_sys_info = Sys.info()
    req_user_agent = httr::user_agent(paste(
        verb,
        "httr",
        local_sys_info[["sysname"]],
        local_sys_info[["version"]]
    ))

    if(is.null(base_url)) base_url = url

    if(is.null(original_headers)) {
        test_req = httr::VERB(verb = verb,url = base_url,req_user_agent)

        if(test_req$status_code != 200) {
            rlang::abort("Connection to server failed!")
        } else {

            req_headers = httr::headers(test_req)
            if(!("domain-id" %in% names(req_headers))){
                req_headers$accept = "*/*"
                # req_headers$`accept-language` = "fr,fr-FR;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"
                req_headers$`access-control-request-headers` = "domain-id"
                req_headers$`domain-id` = extract_subdomain(base_url)
            }

            req_cookies = httr::cookies(test_req)
            h = httr::handle(base_url)

            formated_headers = unlist(req_headers)
            names(formated_headers) = names(req_headers)

            formated_cookies = test_req$cookies$value
            names(formated_cookies) = test_req$cookies$name

            basic_req = httr::VERB(verb = verb,url = url,handle = h,httr::add_headers(formated_headers),query = query,req_user_agent)

            basic_req = httr::RETRY(
                verb = verb,
                url = url,
                httr::add_headers(.headers = formated_headers),
                httr::set_cookies(.cookies = formated_cookies),
                handle = h,
                query = query,
                req_user_agent,
                times = times,
                pause_base = 1,
                pause_cap = pause,
                quiet = TRUE
            )

            .pkg_env$`start-time-session` = Sys.time()
        }
    }

    session = list(
        basic_req = basic_req,
        headers = req_headers,
        formated_headers = formated_headers,
        cookies = req_cookies,
        formated_cookies = formated_cookies,
        handle = h,
        buildID = find_key_in_json(test_req$all_headers,"inv-version"),
        original_headers = original_headers,
        user_agent = req_user_agent
    )

    .pkg_env$`current-session` = session

    return(session)
}





#' Retrieve or Refresh an Active HTTP Client Session
#'
#' @description
#' Returns the current HTTP session stored in \code{.pkg_env}, refreshing
#' it transparently when it has expired (TTL exceeded) or when the target
#' URL has changed. If no valid session exists, a new one is created via
#' \code{\link{create_client_session}}. This function is the recommended
#' entry point for all internal HTTP requests that require session
#' continuity.
#'
#' @param verb A \code{character} string specifying the HTTP method
#'   (e.g., \code{"GET"}, \code{"POST"}). Forwarded to
#'   \code{\link{create_client_session}} when a new session is needed.
#' @param url A \code{character} string giving the target URL. A trailing
#'   slash is appended automatically if absent before comparison with the
#'   stored session URL.
#' @param base_url A \code{character} string for the base URL used when
#'   creating a new session. Defaults to \code{NULL} (falls back to
#'   \code{url} inside \code{\link{create_client_session}}).
#' @param query A named \code{list} of URL query parameters. Defaults to
#'   \code{NULL}.
#' @param original_headers A named \code{character} vector of custom HTTP
#'   headers forwarded to \code{\link{create_client_session}} if a new
#'   session must be created. Defaults to \code{NULL}.
#' @param session A session \code{list} as returned by
#'   \code{\link{create_client_session}}. Defaults to
#'   \code{.pkg_env$`current-session`} (the package-level cached session).
#' @param pause A \code{numeric} scalar (seconds) capping the retry
#'   back-off. Defaults to \code{3}. Forwarded to
#'   \code{\link{create_client_session}}.
#' @param times A \code{numeric} scalar giving the maximum number of retry
#'   attempts. Defaults to \code{5}. Forwarded to
#'   \code{\link{create_client_session}}.
#'
#' @return A valid session \code{list} (see \code{\link{create_client_session}}
#'   for the full structure), guaranteed to correspond to the requested
#'   \code{url} and to be within its TTL at the time of return.
#'
#' @details
#' The function applies the following decision logic in order:
#' \enumerate{
#'   \item \strong{TTL / NULL check}: if \code{session} is \code{NULL}
#'         \emph{or} the elapsed time since
#'         \code{.pkg_env$`start-time-session`} exceeds
#'         \code{.pkg_env$`life-time-session`}, a new session is created
#'         via \code{\link{create_client_session}}.
#'   \item \strong{URL normalisation}: a trailing slash is appended to
#'         \code{url} if the last character is not \code{"/"}.
#'   \item \strong{URL mismatch check}: if the normalised \code{url}
#'         differs from \code{session$basic_req$url}, the session is
#'         refreshed for the new target.
#' }
#'
#' @section Session Lifetime:
#' TTL is governed by \code{.pkg_env$`life-time-session`}, which should
#' be set at package load time (e.g., in \code{.onLoad()}). The comparison
#' is \code{Sys.time() - .pkg_env$`start-time-session` >
#' .pkg_env$`life-time-session`}.
#'
#' @section Side Effects:
#' When a new session is created, \code{\link{create_client_session}}
#' updates both \code{.pkg_env$`current-session`} and
#' \code{.pkg_env$`start-time-session`} as a side effect.
#'
#' @examples
#' \dontrun{
#' # First call: creates a fresh session
#' session <- active_client_session(
#'   verb = "GET",
#'   url  = "https://www.investing.com"
#' )
#'
#' # Subsequent call: returns cached session if still valid
#' session <- active_client_session(
#'   verb = "GET",
#'   url  = "https://www.investing.com"
#' )
#'
#' # Different URL: forces a session refresh
#' session <- active_client_session(
#'   verb     = "GET",
#'   url      = "https://api.investing.com/",
#'   base_url = "https://www.investing.com"
#' )
#' }
#'
#' @seealso
#' \code{\link{create_client_session}},
#' \code{\link[httr]{RETRY}}
#'
#' @keywords internal
active_client_session = function(verb,url, base_url = NULL, query = NULL,original_headers = NULL, session = .pkg_env$`current-session`,pause = 3,times = 5) {

    if ((Sys.time() - .pkg_env$`start-time-session` > .pkg_env$`life-time-session`) || is.null(session)) {
        session = create_client_session(verb = verb, url = url, base_url = base_url,query = query, original_headers = original_headers,pause = pause,times = times)
    }

    # Ensure trailing slash
    if (substr(url, nchar(url), nchar(url)) != "/") {
        url = paste0(url, "/")
    }

    # Refresh session if URL changed
    if (session$basic_req$url != url) {
        session = create_client_session(verb = verb, url = url, base_url = base_url,query = query, original_headers = original_headers,pause = pause,times = times)
    }

    return(session)
}
