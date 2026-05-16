#' Retrieve the Last Downloaded Dataset from Cache
#'
#' @description
#' Returns the most recently downloaded dataset stored in the internal package
#' cache (\code{.pkg_env$last_data_downloaded}). This cache is updated
#' automatically after every successful call to \code{\link{GET_data}}.
#'
#' @return The last downloaded object (typically a \code{data.frame} or a named
#'   \code{list} depending on the \code{output_format} used in
#'   \code{\link{GET_data}}), or \code{NULL} if no data has been retrieved yet
#'   in the current session.
#'
#' @details
#' The cache entry \code{.pkg_env$last_data_downloaded} is a session-level
#' store — it is reset each time a new R session starts. It is intended for
#' quick re-access to the last fetched dataset without repeating a network
#' request.
#'
#' @seealso \code{\link{GET_data}}, \code{\link{GET_tickers}}
#'
#' @examples
#' \dontrun{
#' # Fetch data first
#' GET_data("BRVM", ticker = "BICC")
#'
#' # Then recover the cached result
#' last <- RECOVER_last_download()
#' head(last)
#' }
#'
#' @export
RECOVER_last_download <- function() {
    cache <- .pkg_env$last_data_downloaded
    if (is.null(cache)) {
        message(
            "[RECOVER_last_download] No data in cache yet. ",
            "Call GET_data() first."
        )
        return(invisible(NULL))
    }
    return(cache)
}
