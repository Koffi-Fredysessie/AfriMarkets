#' Plot OHLCV Financial Charts Using Highcharter
#'
#' @description
#' A generic S4 function that produces interactive financial charts (candlestick
#' with volume, or multi-ticker line charts) using the \pkg{highcharter} library.
#' Two methods are available:
#' \itemize{
#'   \item \strong{\code{data.frame} method}: accepts a pre-loaded OHLCV
#'         \code{data.frame} and renders the chart directly.
#'   \item \strong{\code{character} method}: accepts a market code and one or
#'         more ticker symbols, fetches data automatically via \code{GET_data()},
#'         then delegates to the \code{data.frame} method. When multiple tickers
#'         are supplied via \code{y}, all series are fetched and combined before
#'         rendering, producing a grouped multi-ticker chart.
#' }
#'
#' @param x The primary input. Either:
#'   \itemize{
#'     \item A \code{data.frame} with required OHLCV columns (see Details), or
#'     \item A \code{character} string representing the market code (e.g.,
#'           \code{"BRVM"}, \code{"NYSE"}).
#'   }
#' @param ticker \code{[data.frame method]} An optional \code{character} string
#'   used as a fallback ticker label. Ignored when the \code{Ticker} column is
#'   already present in \code{x}.
#' @param y \code{[character method only]} A \code{character} vector of one or
#'   more ticker symbols to retrieve from the given market. Each element is
#'   passed to \code{GET_data()} individually and the results are row-bound
#'   before plotting. Example: \code{y = c("BOAB", "BICC", "SIVC")}.
#' @param from \code{[character method only]} A \code{Date} or \code{character}
#'   string (coercible to \code{Date}) indicating the start of the data
#'   retrieval window. Defaults to \code{Sys.Date() - 89} (last 90 days).
#' @param to \code{[character method only]} A \code{Date} or \code{character}
#'   string indicating the end of the data retrieval window. Defaults to
#'   \code{Sys.Date()}.
#' @param up.col A \code{character} string specifying the colour for upward
#'   price movements (i.e., \code{Close > lag(Close)}). Defaults to
#'   \code{"darkgreen"}. Applies to both candlestick bodies and volume bars.
#' @param down.col A \code{character} string specifying the colour for downward
#'   price movements (i.e., \code{Close <= lag(Close)}). Defaults to
#'   \code{"red"}. Applies to both candlestick bodies and volume bars.
#' @param ... Additional arguments passed to further methods (currently unused).
#'
#' @return A \code{highchart} object (from \pkg{highcharter}) representing an
#'   interactive stock chart. The exact chart type depends on the number of
#'   unique tickers in the data:
#'   \describe{
#'     \item{Single ticker}{A stock chart with:
#'       \itemize{
#'         \item A candlestick series (OHLC data via \code{xts}).
#'         \item A volume column series below, colour-coded by price direction
#'               (\code{"up"}, \code{"down"}, \code{"flat"}).
#'         \item A dual Y-axis layout: price (75\% height) and volume (20\%
#'               height).
#'         \item A range selector and export button.
#'       }
#'     }
#'     \item{Multiple tickers}{A stock chart with:
#'       \itemize{
#'         \item One candlestick series per ticker.
#'         \item A grouped line series of closing prices, one line per ticker.
#'         \item A title listing all ticker symbols and the shared date range.
#'         \item A range selector and export button.
#'       }
#'     }
#'   }
#'   Returns \code{NULL} silently on error, with the condition message emitted
#'   via \code{message()}.
#'
#' @details
#' \strong{data.frame method} workflow:
#' \enumerate{
#'   \item \strong{Validation}: checks that all required columns are present:
#'         \code{Date}, \code{Open}, \code{High}, \code{Low}, \code{Close},
#'         \code{Volume}, \code{Ticker}. Aborts with \code{rlang::abort()} if
#'         any are missing.
#'   \item \strong{Preprocessing}: converts \code{Date} to \code{Date} class,
#'         groups by \code{Ticker}, sorts chronologically, and adds a
#'         \code{Direction} column (\code{"up"} / \code{"down"} / \code{"flat"})
#'         based on \code{Close} vs \code{lag(Close)}.
#'   \item \strong{Single-ticker branch}: converts the \code{data.frame} to an
#'         \code{xts} object and renders a dual-panel candlestick + volume chart.
#'   \item \strong{Multi-ticker branch}: renders a combined candlestick and
#'         grouped close-price line chart for all tickers simultaneously.
#' }
#'
#' \strong{character method} workflow:
#' \enumerate{
#'   \item Iterates over each element of \code{y} and calls
#'         \code{GET_data(market_code = x, ticker = y[i], from = from, to = to,
#'         output_format = "all")} to retrieve OHLCV data.
#'   \item Row-binds all successfully retrieved \code{data.frame} objects into
#'         a single combined dataset.
#'   \item If at least one ticker returned valid data, delegates to
#'         \code{pplot(x = combined_data, ...)}.
#'   \item Otherwise, aborts with an informative error message listing all
#'         tickers that could not be fetched.
#' }
#'
#' @section Required Columns (data.frame method):
#' The input \code{data.frame} must contain exactly the following columns
#' (case-sensitive):
#' \tabular{ll}{
#'   \strong{Column} \tab \strong{Description} \cr
#'   \code{Date}     \tab Trade date (coerced to \code{Date}) \cr
#'   \code{Open}     \tab Opening price \cr
#'   \code{High}     \tab Intraday high price \cr
#'   \code{Low}      \tab Intraday low price \cr
#'   \code{Close}    \tab Closing price \cr
#'   \code{Volume}   \tab Trading volume \cr
#'   \code{Ticker}   \tab Asset ticker symbol (\code{character}) \cr
#' }
#'
#' @section Chart Layout (single ticker):
#' \preformatted{
#' +------------------------------------+
#' |   Candlestick (Price)   [75% h]   |
#' +------------------------------------+
#' |   Volume bars           [20% h]   |
#' +------------------------------------+
#' }
#' Volume bars inherit the direction colour: \code{up.col} when price rose,
#' \code{down.col} when it fell, and the default Highcharts colour when flat.
#'
#' @section Multi-Ticker Support:
#' The \code{character} method accepts a \code{character} \strong{vector} for
#' \code{y}, enabling simultaneous visualisation of multiple assets from the
#' same market:
#' \preformatted{
#' pplot(x = "BRVM", y = c("BOAB", "BICC", "SIVC"),
#'       from = "2024-01-01", to = Sys.Date())
#' }
#' Tickers that fail to return data are skipped with a warning; plotting
#' proceeds as long as at least one ticker succeeds.
#'
#' @section Error Handling:
#' Both methods wrap their logic in \code{tryCatch()}:
#' \itemize{
#'   \item \strong{Errors}: caught, message emitted via \code{message()},
#'         \code{NULL} returned invisibly.
#'   \item \strong{Warnings}: caught and re-emitted via \code{message()}.
#'   \item \code{rlang::abort()} is used for anticipated failures (missing
#'         columns, failed data fetch) to produce structured conditions.
#' }
#'
#' @importFrom rlang abort
#' @importFrom dplyr mutate group_by arrange case_when lag bind_rows
#' @importFrom xts xts
#' @importFrom highcharter highchart hc_title hc_add_series hcaes
#'   hc_add_yAxis hc_yAxis_multiples hc_rangeSelector hc_exporting hc_xAxis
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # ---- data.frame method: single ticker ----
#' df <- data.frame(
#'   Date   = seq(as.Date("2024-01-01"), by = "day", length.out = 60),
#'   Open   = runif(60, 100, 110),
#'   High   = runif(60, 110, 120),
#'   Low    = runif(60, 90,  100),
#'   Close  = runif(60, 100, 115),
#'   Volume = sample(1e6:5e6, 60),
#'   Ticker = "BOAB"
#' )
#' pplot(x = df, up.col = "darkgreen", down.col = "red")
#'
#' # ---- data.frame method: multiple tickers ----
#' df_multi <- dplyr::bind_rows(
#'   data.frame(
#'     Date   = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
#'     Open   = runif(30, 100, 110), High = runif(30, 110, 120),
#'     Low    = runif(30, 90,  100), Close = runif(30, 100, 115),
#'     Volume = sample(1e6:5e6, 30), Ticker = "BOAB"
#'   ),
#'   data.frame(
#'     Date   = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
#'     Open   = runif(30, 200, 210), High = runif(30, 210, 220),
#'     Low    = runif(30, 190, 200), Close = runif(30, 200, 215),
#'     Volume = sample(1e6:5e6, 30), Ticker = "BICC"
#'   )
#' )
#' pplot(x = df_multi)
#'
#' # ---- character method: single ticker ----
#' pplot(
#'   x    = "BRVM",
#'   y    = "BICC",
#'   from = as.Date("2024-01-01"),
#'   to   = Sys.Date()
#' )
#'
#' # ---- character method: multiple tickers simultaneously ----
#' pplot(
#'   x    = "BRVM",
#'   y    = c("BOAB", "BICC", "SIVC"),
#'   from = as.Date("2024-01-01"),
#'   to   = Sys.Date(),
#'   up.col   = "steelblue",
#'   down.col = "tomato"
#' )
#' }
#'
#' @seealso
#' \code{\link[highcharter]{highchart}},
#' \code{\link[xts]{xts}},
#' \code{\link[dplyr]{mutate}},
#' \code{\link{GET_data}}
#'
#' @rdname pplot
#' @aliases pplot,data.frame-method pplot,character-method
#' @export
setGeneric("pplot", function(x, ...) standardGeneric("pplot"))


# ==============================================================================
# METHOD 1 : data.frame
# ==============================================================================

#' @rdname pplot
#' @export
setMethod(
    "pplot",
    signature(x = "data.frame"),
    function(x, ticker = NULL, up.col = "darkgreen", down.col = "red", ...) {

        tryCatch({

            # ========================= VALIDATION
            required_cols <- c("Date", "Open", "High", "Low", "Close", "Volume", "Ticker")
            missing_cols  <- setdiff(required_cols, colnames(x))
            if (length(missing_cols) > 0) {
                rlang::abort(
                    paste0(
                        "The data.frame is missing required column(s): ",
                        paste(missing_cols, collapse = ", "), "."
                    )
                )
            }

            # ========================= PREPROCESSING
            x <- x %>%
                dplyr::mutate(Date = as.Date(Date)) %>%
                dplyr::group_by(Ticker) %>%
                dplyr::arrange(Date, .by_group = TRUE) %>%
                dplyr::mutate(
                    Direction = dplyr::case_when(
                        Close > dplyr::lag(Close) ~ "up",
                        Close < dplyr::lag(Close) ~ "down",
                        TRUE                      ~ "flat"
                    )
                ) %>%
                dplyr::ungroup()

            date1    <- min(x$Date)
            date2    <- max(x$Date)
            tickers  <- unique(x$Ticker)
            n_ticker <- length(tickers)

            # ========================= SINGLE TICKER
            if (n_ticker == 1L) {

                ticker <- tickers[[1L]]

                ohlcv_xts <- xts::xts(
                    x          = x[, c("Open", "High", "Low", "Close", "Volume")],
                    order.by   = x$Date
                )

                market.pplot <- highcharter::highchart(type = "stock") %>%
                    highcharter::hc_title(
                        text = paste0(ticker, " | from ", date1, " to ", date2)
                    ) %>%
                    highcharter::hc_add_series(
                        ohlcv_xts,
                        name    = ticker,
                        upColor = up.col,
                        color   = down.col
                    ) %>%
                    highcharter::hc_add_yAxis(nid = 1L, relative = 1) %>%
                    highcharter::hc_add_series(
                        data = x,
                        type = "column",
                        yAxis = 1L,
                        name  = "Volume",
                        highcharter::hcaes(x = Date, y = Volume, group = Direction)
                    ) %>%
                    highcharter::hc_yAxis_multiples(
                        list(title = list(text = "Price"),  height = "75%"),
                        list(title = list(text = "Volume"), height = "20%",
                             top = "80%", offset = 0)
                    ) %>%
                    highcharter::hc_rangeSelector(enabled = TRUE) %>%
                    highcharter::hc_exporting(
                        enabled  = TRUE,
                        filename = paste0(ticker, "_chart")
                    )

            } else {

                # ========================= MULTI TICKER
                market.pplot <- highcharter::highchart(type = "stock") %>%
                    highcharter::hc_title(
                        text = paste0(
                            "Tickers: ", paste(tickers, collapse = ", "),
                            " | from ", date1, " to ", date2
                        )
                    ) %>%
                    highcharter::hc_add_series(
                        data  = x,
                        type  = "candlestick",
                        highcharter::hcaes(
                            x     = Date,
                            open  = Open,
                            high  = High,
                            low   = Low,
                            close = Close,
                            group = Ticker
                        )
                    ) %>%
                    highcharter::hc_add_series(
                        data  = x,
                        type  = "line",
                        highcharter::hcaes(x = Date, y = Close, group = Ticker)
                    ) %>%
                    highcharter::hc_xAxis(title = list(text = "")) %>%
                    highcharter::hc_rangeSelector(enabled = TRUE) %>%
                    highcharter::hc_exporting(
                        enabled  = TRUE,
                        filename = paste0(
                            paste(tickers, collapse = "_"), "_chart"
                        )
                    )
            }

            return(market.pplot)

        }, error = function(e) {
            message("[pplot] Error: ", conditionMessage(e))
            return(invisible(NULL))
        }, warning = function(w) {
            message("[pplot] Warning: ", conditionMessage(w))
            return(invisible(NULL))
        })
    }
)


# ==============================================================================
# METHOD 2 : character  — supporte y = c("BOAB", "BICC", ...)
# ==============================================================================

#' @rdname pplot
#' @export
setMethod(
    "pplot",
    signature(x = "character"),
    function(x, y, from = Sys.Date() - 89, to = Sys.Date(),
             up.col = "darkgreen", down.col = "red", ...) {

        tryCatch({

            # ---- normalisation des dates ----------------------------------------
            from <- as.Date(from)
            to   <- as.Date(to)

            if (is.na(from) || is.na(to)) {
                rlang::abort("`from` and `to` must be coercible to Date.")
            }
            if (from > to) {
                rlang::abort("`from` must be earlier than or equal to `to`.")
            }

            # ---- validation de y ------------------------------------------------
            if (missing(y) || !is.character(y) || length(y) == 0L) {
                rlang::abort("`y` must be a non-empty character vector of ticker symbols.")
            }

            y <- trimws(unique(y))   # dédoublonner + nettoyer les espaces

            # ---- récupération des données pour chaque ticker --------------------
            collected   <- vector("list", length(y))
            failed      <- character(0L)

            for (i in seq_along(y)) {
                ticker_i <- y[[i]]
                result   <- tryCatch(
                    GET_data(
                        market_code   = x,
                        ticker        = ticker_i,
                        from          = from,
                        to            = to,
                        output_format = "all"
                    ),
                    error   = function(e) NULL,
                    warning = function(w) NULL
                )

                if (is.list(result) && is.data.frame(result$by_col) &&
                    nrow(result$by_col) > 0L) {
                    collected[[i]] <- result$by_col
                } else {
                    failed <- c(failed, ticker_i)
                    message(
                        "[pplot] Warning: no data retrieved for ticker '",
                        ticker_i, "' on market '", x, "'. Skipping."
                    )
                }
            }

            # ---- vérification qu'au moins un ticker a retourné des données -----
            collected <- Filter(Negate(is.null), collected)

            if (length(collected) == 0L) {
                rlang::abort(
                    paste0(
                        "No data could be retrieved for any of the requested ",
                        "ticker(s): ", paste(y, collapse = ", "),
                        " on market '", x, "' between ", from, " and ", to, "."
                    )
                )
            }

            # ---- combinaison et délégation à la méthode data.frame -------------
            combined_data <- dplyr::bind_rows(collected)

            market.pplot <- pplot(
                x        = combined_data,
                up.col   = up.col,
                down.col = down.col,
                ...
            )

            return(market.pplot)

        }, error = function(e) {
            message("[pplot] Error: ", conditionMessage(e))
            return(invisible(NULL))
        }, warning = function(w) {
            message("[pplot] Warning: ", conditionMessage(w))
            return(invisible(NULL))
        })
    }
)
