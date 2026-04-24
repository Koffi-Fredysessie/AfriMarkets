#' Stationarity test for univariate data
#'
#' @description
#' Performs multiple statistical tests to assess the stationarity of a univariate
#' numeric vector or time series. The function supports several commonly used
#' stationarity tests and can return results for one or multiple methods.
#'
#' @details
#' The function evaluates stationarity using different statistical tests depending
#' on the selected \code{type.test}. If \code{"ALL"} is specified, all available
#' tests are performed.
#'
#' Missing values are automatically removed before applying the tests.
#'
#' Available methods include:
#' \itemize{
#'   \item \code{"Box-Pierce and Ljung-Box"} — autocorrelation test
#'   \item \code{"Kwiatkowski-Phillips-Schmidt-Shin (KPSS)"} — stationarity around a deterministic trend
#'   \item \code{"Augmented Dickey-Fuller Test (ADF)"} — unit root test
#'   \item \code{"Phillips-Perron Unit Root Test"} — unit root test robust to serial correlation
#' }
#'
#' Each test has a minimum sample size requirement. If the condition is not met,
#' the corresponding result will return \code{NA}.
#'
#' @param x A numeric vector or a time series object (\code{ts}).
#' @param type.test A character vector specifying which test(s) to perform.
#' Accepted values include:
#' \code{"Box-Pierce and Ljung-Box"},
#' \code{"Kwiatkowski-Phillips-Schmidt-Shin (KPSS)"},
#' \code{"Augmented Dickey-Fuller Test (ADF)"},
#' \code{"Phillips-Perron Unit Root Test"},
#' or \code{"ALL"} for all tests.
#'
#' @return
#' A named list containing p-values for each selected test.
#' Returns \code{NA} if the input data is constant or invalid.
#'
#' @seealso \code{\link{normality_test}}
#'
#' @family statistical_tests
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @importFrom methods setGeneric setMethod
#' @importFrom tseries kpss.test adf.test pp.test
#'
#' @examples
#' \donttest{
#' library(tseries)
#'
#' x <- ts(c(100, 102, 101, 105, 107, NA, 110, 108))
#' x1 = impute.data(x)
#' # Single test
#' stationarity_test(x1, "Augmented Dickey-Fuller Test (ADF)")
#'
#' # Multiple tests
#' stationarity_test(x1, c("kpss", "phillips-perron unit root test"))
#'
#' # All tests
#' stationarity_test(x1, "ALL")
#' }
#'
#' @rdname stationarity_test
#' @export
setGeneric("stationarity_test",
           function(x, type.test = "ALL")
               standardGeneric("stationarity_test"))
#' @rdname stationarity_test
#' @export
#' @importFrom stats is.ts
# Fonction interne factorisée
.stationarity_core <- function(x, type.test) {

    # Nettoyage NA
    if (is.ts(x)) {
        x <- stats::na.omit(x)
    } else {
        x <- stats::na.omit(x)
    }

    # Si vecteur constant ou vide
    if (length(x) <= 1 || length(unique(x)) <= 1) {
        return(NA)
    }

    # Normalisation des tests
    if (tolower(type.test[1]) == "all") {
        type.test <- c(
            "box-pierce and ljung-box",
            "kwiatkowski-phillips-schmidt-shin (kpss)",
            "augmented dickey-fuller test (adf)",
            "phillips-perron unit root test"
        )
    }

    type.test <- tolower(type.test)

    results <- list()

    # ---- Box-Pierce / Ljung-Box ----
    if ("box-pierce and ljung-box" %in% type.test) {
        pval <- if (length(x) >= 3) stats::Box.test(x)$p.value else NA
        results[["box-pierce and ljung-box"]] <- as.numeric(pval)
    }

    # ---- KPSS ----
    if ("kwiatkowski-phillips-schmidt-shin (kpss)" %in% type.test) {
        pval <- if (length(x) >= 3) tseries::kpss.test(x)$p.value else NA
        results[["kwiatkowski-phillips-schmidt-shin (kpss)"]] <- as.numeric(pval)
    }

    # ---- ADF ----
    if ("augmented dickey-fuller test (adf)" %in% type.test) {
        pval <- if (length(x) >= 7) tseries::adf.test(x)$p.value else NA
        results[["augmented dickey-fuller test (adf)"]] <- as.numeric(pval)
    }

    # ---- Phillips-Perron ----
    if ("phillips-perron unit root test" %in% type.test) {
        pval <- if (length(x) >= 4) tseries::pp.test(x)$p.value else NA
        results[["phillips-perron unit root test"]] <- as.numeric(pval)
    }

    return(results)
}
#' @rdname stationarity_test
#' @export
# Méthode pour numeric
setMethod("stationarity_test",
          signature(x = "numeric"),
          function(x, type.test = "ALL") {
              .stationarity_core(x, type.test)
          })
#' @rdname stationarity_test
#' @export
# Méthode pour ts
setMethod("stationarity_test",
          signature(x = "ts"),
          function(x, type.test = "ALL") {
              .stationarity_core(x, type.test)
          })

