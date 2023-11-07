#' Plot Values of Dataset or VAR Residuals
#'
#' Plots the historic values of variables in a dataset, or residuals of a VAR model. \code{ggvar_values} Plots each series in a facet. \code{ggvar_values_colored} plots all in the same graph, each with a different color.
#'
#' @param x Either a "varest" object for plotting the residual values, or an dataset (object coercible to data.frame) with numeric variables and possibly a date/index column. If provided a index column (not a date one), it must be called "Date".
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param index A vector of labels to the x-axis, normally dates. Must have length equal to \code{x$obs} or \code{nrow(x)}. Defaults to a numeric sequence.
#' @param palette A vector of colors. Just one for \code{ggvar_values}, one for each variable for \code{ggvar_values_colored}. See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Aditional arguments passed to \link[ggplot2]{geom_line}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_values(EuStockMarkets)
#' ggvar_values_colored(EuStockMarkets)
#' ggvar_values(vars::VAR(EuStockMarkets))
#'
#' @export
ggvar_values <- function(
    x, series = NULL, index = NULL,
    palette = c("black"), scales = "fixed", ncol = 1, ...
  ){
  # Initial tests:
  stopifnot(inherits(x, c("data.frame", "matrix", "varest")))

  # Create values:
  palette <- get_pallete(palette, 1)

  if (inherits(x, "varest")) {
    title <- "VAR Residuals"
    data <- as.data.frame(stats::residuals(x))
  } else {
    title <- "Historic Values"
    data <- as.data.frame(x)
  }

  series <- series %||% colnames(data)
  index <- index %||% 1:nrow(data)

  # Data - pivoting:
  data_values <- data %>%
    dplyr::mutate(index = index) %>%
    tidyr::pivot_longer(-.data$index, values_to = "value", names_to = "serie")

  # Graph:
  ggplot(data_values, aes(.data$index, .data$value)) +
    ggplot2::geom_line(color = palette[1], ...) +
    ggplot2::facet_wrap(vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = title, x = "Index", y = "Values")
}

#' @rdname ggvar_values
#' @export
ggvar_values_colored <- function(
    x, series = NULL, index = NULL,
    palette = NULL, ...
  ) {
  # Initial tests:
  stopifnot(inherits(x, c("data.frame", "matrix", "varest")))

  # Create values:
  if (inherits(x, "varest")) {
    title <- "VAR Residuals"
    data <- as.data.frame(stats::residuals(x))
  } else {
    title <- "Historic Values"
    data <- as.data.frame(x)
  }

  series <- series %||% colnames(data)
  palette <- get_pallete(palette, length(series))
  index <- index %||% 1:nrow(data)

  # Data - pivoting:
  data_values <- data %>%
    dplyr::mutate(index = index) %>%
    tidyr::pivot_longer(-.data$index, values_to = "value", names_to = "serie")

  # Graph:
  ggplot(data_values, aes(.data$index, .data$value)) +
    ggplot2::geom_line(aes(color = serie), ...) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(title = title, x = "Index", y = "Values")
}
