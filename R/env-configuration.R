#' Internal environment for package state management
#'
#' @description
#' This internal environment is used to store temporary data such as API cache,
#' authentication tokens, and other runtime objects. It is not exported and
#' should not be accessed directly by users.
#'
#' @keywords internal
#' @noRd
# Set local Env
.pkg_env = new.env(parent = emptyenv())

# Session
.pkg_env$`current-session` = NULL
.pkg_env$build_id = NULL
.pkg_env$`start-time-session`= Sys.time()
.pkg_env$`life-time-session`= 1 #1800 # 1800 sec

# Cache AfriMarkets for 'investing.com'
.pkg_env$last_cache_operation_is_available = FALSE
.pkg_env$last_ticker_downloaded = NULL
.pkg_env$last_data_downloaded = NULL





#' Get current system time (numeric)
#'
#' @description
#' Returns the current system time as a numeric value (seconds since Unix epoch).
#' This is a lightweight wrapper around \code{Sys.time()} commonly used for
#' time comparisons, caching, and TTL (time-to-live) calculations.
#'
#' @return A numeric value representing the current time in seconds since
#'   January 1, 1970 (Unix epoch).
#'
#' @details
#' This function is typically used internally for:
#' \itemize{
#'   \item Cache expiration checks
#'   \item Timestamp comparisons
#'   \item Performance timing
#' }
#'
#' @examples
#' getSystime()
#'
#' @keywords internal
#' @rdname getSystime
#' @export
getSystime <- function() {
    as.numeric(Sys.time())
}
