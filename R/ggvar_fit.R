#' @noRd
setup_tests_ggvar_fit <- function(x, compare, series, index) {
  test$class_arg(x, c("varest"))
  test$series(series, x)
  test$index(index, x, n = x$totobs)
  test$boolean_arg(compare)
}

#' @noRd
setup_data_ggvar_fit <- function(x, compare, series, index) {
  data.frame(index = index[(x$p + 1):x$totobs], Fitted.. = stats::fitted(x)) %>%
    `if`(compare, cbind(., Original.. = x$datamat[1:x$K]), .) %>%
    dplyr::select(c(index, dplyr::ends_with(series))) %>%
    tidyr::pivot_longer(-index, names_to = c("type", "serie"), names_sep = "\\.\\.\\.",  values_to = "value")
}

#' Plot VAR Fitted Values
#'
#' Plots fitted values of a VAR model, versus the actual values. \code{ggvar_fit} Plots each series in a facet. \code{ggvar_fit_colored} plots all in the same graph, each with a different color.
#'
#' @param x A "varest" object to get fitted values from.
#' @param compare Logical, should the true values be printed?
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param index A vector of labels to the x-axis, normally dates. Must have length equal to \code{x$obs}. Defaults to a numeric sequence.
#' @param palette A vector of colors. Just one for \code{ggvar_fit}, one for each variable for \code{ggvar_fit_colored}. See \code{vignette("palettes")}.
#' @param linetypes A vector of line types (fitted, original), passed to \link[ggplot2]{scale_linetype_manual}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An integer. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Additional arguments passed to \link[ggplot2]{geom_line}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' x <- vars::VAR(EuStockMarkets)
#' ggvar_fit(x, index = stats::time(EuStockMarkets))
#' ggvar_fit_colored(x, index = stats::time(EuStockMarkets))
#'
#' @export
ggvar_fit <- function(
    x, compare = TRUE, series = NULL, index = 1:x$totobs,
    palette = c("black"), linetypes = c("solid", "dashed"), scales = "fixed", ncol = 1, ...
  ) {
  # Initial tests:
  setup_tests_ggvar_fit(x, compare, series, index)

  # Create values:
  series <- series %||% names(x$varresult)
  palette <- get_pallete(palette, 1)

  # Data:
  data <- setup_data_ggvar_fit(x, compare, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(aes(linetype = .data$type), color = palette, ...) +
    ggplot2::facet_wrap(vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = "Fitted VAR Values") +
    ggplot2::scale_linetype_manual(values = linetypes, guide = if (compare) "legend" else "none")
}

#' @rdname ggvar_fit
#' @export
ggvar_fit_colored <- function(
    x, compare = TRUE, series = NULL, index = 1:x$obs,
    palette = NULL, linetypes = c("solid", "dashed"), ...
  ) {
  # Initial tests:
  setup_tests_ggvar_fit(x, compare, series, index)

  # Create values:
  series <- series %||% names(x$varresult)
  palette <- get_pallete(palette, length(series))

  # Data:
  data <- setup_data_ggvar_fit(x, compare, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(aes(color = .data$serie, linetype = .data$type), ...) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_linetype_manual(values = linetypes, guide = if (compare) "legend" else "none") +
    ggplot2::labs(title = "Fitted VAR Values", x = "Index", y = "Fitted")
}
