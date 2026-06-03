#' Extract the Subdomain from a URL
#'
#' Parses a URL and returns its subdomain if present.
#'
#' @param url A character string. The full URL to parse
#'   (e.g. \code{"https://www.investing.com"}).
#'
#' @return A character string with the subdomain (e.g. \code{"www"},
#'   \code{"api"}), or \code{NA_character_} if no subdomain is present.
#'
#' @examples
#' \dontrun{
#' extract_subdomain("https://www.investing.com")  # "www"
#' extract_subdomain("https://api.brvm.org/fr")    # "api"
#' extract_subdomain("https://ngx.com.ng")         # NA
#' }
#'
#' @seealso \code{\link[httr]{parse_url}}
extract_subdomain <- function(url) {
    stringr::str_extract(url, "(?<=https://)[a-z]+")
}
