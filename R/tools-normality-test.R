#' Normality Tests for Univariate Data
#'
#' @description
#' Performs a set of statistical tests to assess the normality of a numeric
#' vector or time series. This function is fully generic and can be applied
#' to any type of data (financial, economic, scientific, etc.), regardless
#' of the underlying market.
#'
#' @details
#' The function evaluates whether the input data follows a normal distribution
#' using one or multiple statistical tests. If \code{type.test = "ALL"},
#' all available tests are automatically performed.
#'
#' The following tests are supported:
#' \itemize{
#'   \item Anderson-Darling
#'   \item Shapiro-Wilk
#'   \item Jarque-Bera
#'   \item Cramer-von Mises
#'   \item Shapiro-Francia
#'   \item Lilliefors (Kolmogorov-Smirnov)
#'   \item Pearson Chi-square
#'   \item D'Agostino
#' }
#'
#' Missing values are automatically removed prior to computation.
#' Each test returns a p-value indicating whether the null hypothesis
#' of normality can be rejected.
#'
#' @param x A numeric vector or time series object.
#' @param type.test A character vector specifying which tests to perform.
#' Possible values include:
#' \code{"Anderson-Darling"}, \code{"Shapiro-Wilk"}, \code{"Jarque Bera"},
#' \code{"Cramer-von Mises"}, \code{"Shapiro-Francia"},
#' \code{"Lilliefors (Kolmogorov-Smirnov)"}, \code{"Pearson chi-square"},
#' \code{"Agostino"}, or \code{"ALL"} (default).
#'
#' @return A named list of p-values corresponding to each selected test.
#' Returns \code{NA} if the input data has no variability (constant values).
#'
#' @note
#' This function is market-agnostic and can be used across all datasets
#' handled within the AfriMarkets ecosystem.
#'
#' @family statistical_tests
#'
#' @author
#' Koffi Frederic SESSIE \cr
#' Olabiyi Aurel Geoffroy ODJO
#'
#' @seealso \code{\link{stationarity_test}}
#'
#' @examples
#' \donttest{
#' x <- rnorm(100)
#'
#' # Run all tests
#' normality_test(x)
#'
#' # Run selected tests
#' normality_test(x, c("shapiro-wilk", "jarque bera"))
#' }
#'
#' @importFrom stats shapiro.test na.omit
#' @importFrom goftest ad.test cvm.test
#' @importFrom tseries jarque.bera.test
#' @importFrom nortest sf.test lillie.test pearson.test
#' @importFrom fBasics dagoTest
#'
#' @rdname normality_test
.normality_test_core <- function(x, type.test) {

    if (length(unique(x)) <= 1) return(NA)

    # Clean NA
    x <- stats::na.omit(x)

    if (tolower(type.test[1]) == "all") {
        type.test <- c(
            "anderson-darling","shapiro-wilk","jarque bera","cramer-von mises",
            "shapiro-francia","lilliefors (kolmogorov-smirnov)",
            "pearson chi-square","agostino"
        )
    }

    type.test <- tolower(type.test)

    used_method <- list()

    if("anderson-darling" %in% type.test){
        used_method[["anderson-darling"]] <-
            if(length(x) >= 8) goftest::ad.test(x)$p.value else NA
    }

    if("shapiro-wilk" %in% type.test){
        used_method[["shapiro-wilk"]] <-
            if(length(x) >= 3) stats::shapiro.test(x)$p.value else NA
    }

    if("jarque bera" %in% type.test){
        used_method[["jarque bera"]] <-
            if(length(x) >= 3) tseries::jarque.bera.test(x)$p.value else NA
    }

    if("cramer-von mises" %in% type.test){
        used_method[["cramer-von mises"]] <-
            if(length(x) >= 1) goftest::cvm.test(x)$p.value else NA
    }

    if("shapiro-francia" %in% type.test){
        used_method[["shapiro-francia"]] <-
            if(length(x) >= 5) nortest::sf.test(x)$p.value else NA
    }

    if("lilliefors (kolmogorov-smirnov)" %in% type.test){
        used_method[["lilliefors (kolmogorov-smirnov)"]] <-
            if(length(x) >= 5) nortest::lillie.test(x)$p.value else NA
    }

    if("pearson chi-square" %in% type.test){
        used_method[["pearson chi-square"]] <-
            if(length(x) >= 2) nortest::pearson.test(x)$p.value else NA
    }

    if("agostino" %in% type.test){
        used_method[["agostino"]] <-
            if(length(x) >= 20) (fBasics::dagoTest(x)@test$p.value)[[1]] else NA
    }

    return(used_method)
}
#' @export
#' @rdname normality_test
setGeneric(
    "normality_test",
    function(x, type.test = "ALL")
        standardGeneric("normality_test")
)
#' @export
#' @rdname normality_test
# numeric
setMethod(
    "normality_test",
    signature(x = "numeric"),
    function(x, type.test = "ALL") {
        .normality_test_core(x, type.test)
    }
)
#' @export
#' @rdname normality_test
# ts
setMethod(
    "normality_test",
    signature(x = "ts"),
    function(x, type.test = "ALL") {
        .normality_test_core(as.numeric(x), type.test)
    }
)
