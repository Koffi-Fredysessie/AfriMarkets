# ==============================================================================
# INTERNAL HELPER — not exported
# ==============================================================================

#' @noRd
.stationarity_core <- function(x, type.test) {

    # ---- Nettoyage des NA -----------------------------------------------------
    x <- stats::na.omit(as.numeric(x))

    # ---- Vecteur constant ou insuffisant --------------------------------------
    if (length(x) <= 1L || length(unique(x)) <= 1L) {
        message("[stationarity_test] Input is constant or has insufficient ",
                "observations. Returning NA.")
        return(NA)
    }

    # ---- Normalisation des noms de tests --------------------------------------
    all_tests <- c(
        "box-pierce and ljung-box",
        "kwiatkowski-phillips-schmidt-shin (kpss)",
        "augmented dickey-fuller test (adf)",
        "phillips-perron unit root test"
    )

    if (length(type.test) == 1L && tolower(trimws(type.test)) == "all") {
        type.test <- all_tests
    } else {
        type.test <- tolower(trimws(type.test))
        unknown   <- setdiff(type.test, all_tests)
        if (length(unknown) > 0L) {
            message(
                "[stationarity_test] Unknown test(s) ignored: ",
                paste(unknown, collapse = ", "), ".\n",
                "Available tests: ", paste(all_tests, collapse = ", "), "."
            )
            type.test <- intersect(type.test, all_tests)
        }
        if (length(type.test) == 0L) {
            rlang::abort(
                paste0(
                    "No valid test selected. Available tests:\n",
                    paste0("  - ", all_tests, collapse = "\n")
                )
            )
        }
    }

    n       <- length(x)
    results <- list()

    # ---- Box-Pierce / Ljung-Box (n >= 3) --------------------------------------
    if ("box-pierce and ljung-box" %in% type.test) {
        if (n >= 3L) {
            results[["box-pierce and ljung-box"]] <-
                as.numeric(stats::Box.test(x)$p.value)
        } else {
            message("[stationarity_test] Box-Pierce/Ljung-Box requires n >= 3. ",
                    "Returning NA (n = ", n, ").")
            results[["box-pierce and ljung-box"]] <- NA_real_
        }
    }

    # ---- KPSS (n >= 3) --------------------------------------------------------
    if ("kwiatkowski-phillips-schmidt-shin (kpss)" %in% type.test) {
        if (n >= 3L) {
            results[["kwiatkowski-phillips-schmidt-shin (kpss)"]] <-
                as.numeric(tseries::kpss.test(x)$p.value)
        } else {
            message("[stationarity_test] KPSS test requires n >= 3. ",
                    "Returning NA (n = ", n, ").")
            results[["kwiatkowski-phillips-schmidt-shin (kpss)"]] <- NA_real_
        }
    }

    # ---- ADF (n >= 7) ---------------------------------------------------------
    if ("augmented dickey-fuller test (adf)" %in% type.test) {
        if (n >= 7L) {
            results[["augmented dickey-fuller test (adf)"]] <-
                as.numeric(tseries::adf.test(x)$p.value)
        } else {
            message("[stationarity_test] ADF test requires n >= 7. ",
                    "Returning NA (n = ", n, ").")
            results[["augmented dickey-fuller test (adf)"]] <- NA_real_
        }
    }

    # ---- Phillips-Perron (n >= 4) ---------------------------------------------
    if ("phillips-perron unit root test" %in% type.test) {
        if (n >= 4L) {
            results[["phillips-perron unit root test"]] <-
                as.numeric(tseries::pp.test(x)$p.value)
        } else {
            message("[stationarity_test] Phillips-Perron test requires n >= 4. ",
                    "Returning NA (n = ", n, ").")
            results[["phillips-perron unit root test"]] <- NA_real_
        }
    }

    return(results)
}


# ==============================================================================
# GENERIC
# ==============================================================================

#' Stationarity Tests for Univariate Time Series
#'
#' @description
#' Performs one or more statistical tests to assess the stationarity of a
#' univariate numeric vector or time series object. Results are returned as a
#' named list of p-values, one entry per selected test.
#'
#' @details
#' The following tests are available. Each has a minimum sample size
#' requirement; if the series is too short, the corresponding p-value is
#' \code{NA} and a diagnostic message is emitted.
#'
#' \tabular{lll}{
#'   \strong{Key (type.test)}                          \tab \strong{Null hypothesis}          \tab \strong{Min. n} \cr
#'   \code{"box-pierce and ljung-box"}                 \tab No autocorrelation                \tab 3              \cr
#'   \code{"kwiatkowski-phillips-schmidt-shin (kpss)"} \tab Series is stationary              \tab 3              \cr
#'   \code{"augmented dickey-fuller test (adf)"}       \tab Unit root present (non-stationary)\tab 7              \cr
#'   \code{"phillips-perron unit root test"}           \tab Unit root present (non-stationary)\tab 4              \cr
#' }
#'
#' Matching of \code{type.test} values is \strong{case-insensitive}. Unrecognised
#' test names are silently dropped with a diagnostic message; if none of the
#' supplied names are valid, the function aborts with an informative error.
#'
#' \code{NA} and \code{NaN} values are removed via \code{stats::na.omit()}
#' before any test is applied. Constant series (all values identical) are
#' detected and return \code{NA} immediately.
#'
#' @param x A \code{numeric} vector or a \code{ts} object. \code{NA} values
#'   are removed automatically before testing.
#' @param type.test A \code{character} vector of test name(s) to perform.
#'   Accepted values (case-insensitive):
#'   \itemize{
#'     \item \code{"box-pierce and ljung-box"}
#'     \item \code{"kwiatkowski-phillips-schmidt-shin (kpss)"}
#'     \item \code{"augmented dickey-fuller test (adf)"}
#'     \item \code{"phillips-perron unit root test"}
#'     \item \code{"ALL"} — runs all four tests (default).
#'   }
#'
#' @return
#' A named \code{list} of \code{numeric} p-values, one element per selected
#' test. Each element is \code{NA_real_} when the series does not meet the
#' minimum sample size requirement for that test.
#' Returns \code{NA} (scalar) when \code{x} is constant or has fewer than
#' two distinct observations.
#'
#' @seealso
#' \code{\link{normality_test}},
#' \code{\link[tseries]{kpss.test}},
#' \code{\link[tseries]{adf.test}},
#' \code{\link[tseries]{pp.test}},
#' \code{\link[stats]{Box.test}}
#'
#' @family statistical_tests
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @importFrom rlang abort
#' @importFrom stats na.omit Box.test
#' @importFrom tseries kpss.test adf.test pp.test
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' x_stationary    <- rnorm(100)
#' x_nonstationary <- cumsum(rnorm(100))   # random walk
#'
#' # Single test
#' stationarity_test(x_stationary, "augmented dickey-fuller test (adf)")
#'
#' # Multiple tests (case-insensitive)
#' stationarity_test(
#'   x_nonstationary,
#'   c("kwiatkowski-phillips-schmidt-shin (kpss)",
#'     "phillips-perron unit root test")
#' )
#'
#' # All tests at once
#' stationarity_test(x_stationary, "ALL")
#'
#' # Works on ts objects too
#' stationarity_test(ts(x_stationary, frequency = 12), "ALL")
#' }
#'
#' @rdname stationarity_test
#' @export
setGeneric(
    "stationarity_test",
    function(x, type.test = "ALL") standardGeneric("stationarity_test")
)


# ==============================================================================
# METHOD : numeric
# ==============================================================================

#' @rdname stationarity_test
#' @export
setMethod(
    "stationarity_test",
    signature(x = "numeric"),
    function(x, type.test = "ALL") {
        .stationarity_core(x, type.test)
    }
)


# ==============================================================================
# METHOD : ts
# ==============================================================================

#' @rdname stationarity_test
#' @export
setMethod(
    "stationarity_test",
    signature(x = "ts"),
    function(x, type.test = "ALL") {
        .stationarity_core(x, type.test)
    }
)
