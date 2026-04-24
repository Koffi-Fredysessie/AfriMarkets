

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




