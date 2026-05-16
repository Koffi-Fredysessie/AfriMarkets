

#' Impute Missing Values in a Data Frame
#'
#' @description
#' This generic function imputes missing values (\code{NA}) in numeric columns
#' of a data frame using different strategies. The data is first ordered
#' according to a specified date column to ensure temporal consistency.
#'
#' @details
#' The function operates as follows:
#' \enumerate{
#'   \item The data frame is sorted by the provided date column.
#'   \item Only numeric columns are selected for imputation.
#'   \item Missing values are replaced according to the selected method:
#'   \itemize{
#'     \item \code{"previous"}: Missing values are filled using the last observed value
#'     (Last Observation Carried Forward), followed by backward filling for leading \code{NA}s.
#'     \item \code{"mean"}: Missing values are replaced by the mean of the column.
#'     \item \code{"median"}: Missing values are replaced by the median of the column.
#'   }
#'   \item Columns containing only \code{NA} values are left unchanged.
#' }
#'
#' @param data A \code{data.frame} containing the data to be processed.
#' @param date_field_name A character string specifying the name of the date column
#' @param ... Additional arguments.
#' used to order the data.
#' @param method A character string specifying the imputation method.
#' Available options are \code{"previous"}, \code{"mean"}, and \code{"median"}.
#' Default is \code{"previous"}.
#'
#' @return A \code{data.frame} with missing values imputed in numeric columns.
#'
#' @note
#' This function is designed for time series or ordered data where preserving
#' temporal structure is important.
#'
#' @examples
#' df <- data.frame(
#'   date = as.Date("2024-01-01") + 0:4,
#'   price = c(NA, 10, NA, 15, NA)
#' )
#'
#' impute.data(df, "date", method = "previous")
#' impute.data(df, "date", method = "mean")
#'
#' @seealso \code{\link[tidyr]{fill}}, \code{\link[stats]{median}}, \code{\link[base]{mean}}
#'
#' @export
#' @rdname impute.data
setGeneric("impute.data", function(data,...) standardGeneric("impute.data" ))
#' @rdname impute.data
#' @export
setMethod("impute.data", signature(data = "data.frame"),
          function(data,date_field_name,method = "previous") {
                if(!(date_field_name %in% colnames(data))){
                    rlang::abort(
                        paste("The current dataframe don't have column named :",date_field_name)
                    )
                }


              # Vérification colonne date
              if (!(date_field_name %in% colnames(data))) {
                  rlang::abort(
                      paste("The current dataframe does not have a column named:", date_field_name)
                  )
              }

              # Vérification méthode
              method <- match.arg(method, c("previous", "median", "mean"))

              # Tri par date
              data <- data %>%
                  dplyr::arrange(.data[[date_field_name]])

              # Imputation
              data <- data %>%
                  dplyr::mutate(
                      dplyr::across(
                          dplyr::where(is.numeric),
                          ~ {
                              x <- .

                              # Si toute la colonne est NA
                              if (all(is.na(x))) return(x)

                              # ---- METHOD: previous ----
                              if (method == "previous") {
                                  x <- tidyr::fill(tibble::tibble(x = x), x, .direction = "downup")$x
                              }

                              # ---- METHOD: median ----
                              if (method == "median") {
                                  x[is.na(x)] <- stats::median(x, na.rm = TRUE)
                              }

                              # ---- METHOD: mean ----
                              if (method == "mean") {
                                  x[is.na(x)] <- base::mean(x, na.rm = TRUE)
                              }

                              return(x)
                          }
                      )
                  )

              return(data)
          })
#' @rdname impute.data
.impute.data = function(data, method = "previous") {

    # Vérification méthode
    method <- match.arg(method, c("previous", "median", "mean"))

    x <- data

    # Si toute la colonne est NA
    if (all(is.na(x))) return(x)

    # S'il y a des NA
    if (any(is.na(x))) {

        # ---- METHOD: previous ----
        if (method == "previous") {
            idx <- which(!is.na(x))

            # S'il y a au moins une valeur non NA
            if (length(idx) > 0) {
                # Remplissage constant (last observation carried forward + backward)
                x <- stats::approx(
                    x = idx,
                    y = x[idx],
                    xout = seq_along(x),
                    method = "constant",
                    rule = 2
                )$y
            }
        }

        # ---- METHOD: median ----
        if (method == "median") {
            x[is.na(x)] <- stats::median(x, na.rm = TRUE)
        }

        # ---- METHOD: mean ----
        if (method == "mean") {
            x[is.na(x)] <- base::mean(x, na.rm = TRUE)
        }
    }

    return(x)
}
#' @rdname impute.data
#' @export
setMethod("impute.data", signature(data = "ts"),
          function(data,method = "previous") {
              return(.impute.data(data,method = method))
          })



#' Clean and Convert Values to Numeric
#'
#' Convert a vector to numeric by removing common thousands separators
#' (commas) and coercing the result into numeric format.
#' Warnings generated during coercion are suppressed.
#'
#' @param x A vector (character, factor, or numeric) containing values to convert.
#'
#' @return A numeric vector of the same length as \code{x}. Values that cannot be
#'   converted are returned as \code{NA}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Coerces input values to character.
#'   \item Removes commas used as thousands separators.
#'   \item Converts the cleaned values to numeric using \code{as.numeric()}.
#'   \item Suppresses warnings generated during coercion.
#' }
#'
#' This function is particularly useful for financial datasets where numeric
#' values are often stored as formatted strings (e.g., "1,234.56").
#'
#' Note that non-standard formats (e.g., currency symbols, spaces, or European
#' decimal formats using commas) are not handled and will result in \code{NA}.
#'
#' @examples
#' clean_numeric("1,234.56")
#' # 1234.56
#'
#' clean_numeric(c("10,000", "5,500.25", "NA", "-", "abc"))
#' # 10000 5500.25 NA NA NA
#'
#' clean_numeric(factor(c("1,000", "2,500")))
#' # 1000 2500
#'
#' @seealso \code{\link{as.numeric}}
#'
#' @export
clean_numeric <- function(x) {
    suppressWarnings(
        as.numeric(gsub(",", "", as.character(x)))
    )
}




#' Format Historical Market Data Outputs
#'
#' @description
#' Internal helper function used to standardize and transform historical
#' market data stored as a named list of ticker-specific data frames into
#' multiple output structures suitable for analysis and modeling.
#'
#' It supports conversion into long format, wide format, or both formats
#' simultaneously.
#'
#' @param data A named list of data frames containing historical market data.
#' Each element name must correspond to a ticker symbol.
#'
#' Each data frame must contain at least a \code{Date} column and may include
#' typical OHLCV variables such as \code{Open}, \code{High}, \code{Low},
#' \code{Close}, and \code{Volume}.
#'
#' @param output_format A character string specifying the desired output format:
#' \itemize{
#'   \item \code{"by_col"}: long (tidy) format with an added \code{Ticker} column.
#'   \item \code{"by_row"}: wide format with ticker-prefixed column names.
#'   \item \code{"all"}: returns both formats as a named list.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Iterates over each ticker dataset in the input list.
#'   \item Constructs a long-format dataset by appending a \code{Ticker} column.
#'   \item Builds a wide-format dataset by prefixing variable names with ticker symbols.
#'   \item Merges all ticker datasets by the \code{Date} column.
#'   \item Orders the final outputs chronologically.
#' }
#'
#' The implementation relies on \pkg{purrr} for iteration and reduction
#' and \pkg{dplyr} for data manipulation and sorting.
#'
#' @return
#' Depending on \code{output_format}, returns:
#' \itemize{
#'   \item A data frame in long format (\code{"by_col"}) containing:
#'   \code{Date}, \code{Ticker}, and market variables.
#'
#'   \item A data frame in wide format (\code{"by_row"}) where each
#'   ticker-variable combination becomes a separate column.
#'
#'   \item A named list containing both formats when
#'   \code{output_format = "all"}.
#' }
#'
#' @examples
#' \dontrun{
#' sample_data <- list(
#'   BOAB = data.frame(
#'     Date = as.Date("2025-01-01") + 0:2,
#'     Close = c(5000, 5100, 5200)
#'   ),
#'   SGBCI = data.frame(
#'     Date = as.Date("2025-01-01") + 0:2,
#'     Close = c(12000, 12100, 12200)
#'   )
#' )
#'
#' output_data(sample_data, "by_col")
#' output_data(sample_data, "by_row")
#' output_data(sample_data, "all")
#' }
#'
#' @importFrom purrr imap imap_dfr reduce
#' @importFrom dplyr arrange full_join
#'
#' @keywords internal
output_data = function(data, output_format = c("by_col","by_row","all")){

    output_format = output_format[1]

    by_col <- purrr::imap_dfr(
        data,
        function(x, ticker){
            x$Ticker <- ticker
            x
        }
    ) %>%
        dplyr::arrange(Date, Ticker)


    by_row <- purrr::imap(
        data,
        function(x, ticker){
            names(x) <- ifelse(names(x) == "Date","Date",paste0(ticker, ".", names(x)))
            x
        }
    ) %>% purrr::reduce(full_join,by = "Date")

    df <- switch(
        output_format,
        by_col = by_col,
        by_row = by_row,

        all = list(
            by_col = by_col,
            by_row = by_row
        )
    )

    return(df)

}


