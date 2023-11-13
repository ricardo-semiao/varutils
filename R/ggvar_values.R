#' @noRd
setup_ggvar_values <- function(x, series, index, palette) {
  x <- test$dataset_arg(x)
  test$class_arg(x, c("data.frame", "matrix", "varest"))
  test$series(series, x)

  # Create values:
  if (inherits(x, "varest")) {
    title <- "VAR Residuals Distribution"
    x <- as.data.frame(stats::residuals(x))
  } else {
    title <- "Time Series Distribution"
    x <- as.data.frame(x)
  }

  list(
    x = x,
    index = index %||% seq_len(nrow(x)),
    series = series %||% get_names(x),
    title = title
  )
}

#' @noRd
data_ggvar_values <- function(data, series, index) {
  data %>%
    dplyr::select(dplyr::all_of(series)) %>%
    dplyr::mutate(index = index) %>%
    tidyr::pivot_longer(-c("index"), values_to = "value", names_to = "serie")
}

#' Plot Values of Dataset or VAR Residuals
#'
#' Plots the historic values of variables in a dataset, or residuals of a VAR model. \code{ggvar_values} Plots each series in a facet. \code{ggvar_values_colored} plots all in the same graph, each with a different color.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset (object coercible to data.frame) with numeric variables.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param index A vector of labels to the x-axis, normally dates. Must have length equal to \code{x$obs} or \code{nrow(x)}. Defaults to a numeric sequence.
#' @param palette A vector of colors. Just one for \code{ggvar_values}, one for each variable for \code{ggvar_values_colored}. See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An integer. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Additional arguments passed to \link[ggplot2]{geom_line}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_values(freeny[-2], scales = "free_y")
#' ggvar_values_colored(freeny[-2])
#' ggvar_values(vars::VAR(freeny[-2]), scales = "free_y")
#'
#' @export
ggvar_values <- function(
    x, series = NULL, index = NULL,
    palette = c("black"), scales = "fixed", ncol = 1, ...) {
  # Setup:
  setup <- setup_ggvar_values(x, series, index, palette)
  reassign <- c("x", "series", "index", "palette")
  list2env(setup[reassign], envir = rlang::current_env())

  test$index(index, n = nrow(x), "'obs' of `x` in varest form")
  palette = get_pallete(palette, 1)

  # Data:
  data <- data_ggvar_values(x, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(color = palette[1], ...) +
    ggplot2::facet_wrap(vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = setup$title, x = "Index", y = "Values")
}

#' @rdname ggvar_values
#' @export
ggvar_values_colored <- function(
    x, series = NULL, index = NULL,
    palette = NULL, ...) {
  # Setup:
  setup <- setup_ggvar_values(x, series, index, palette)
  reassign <- c("x", "series", "index", "palette")
  list2env(setup[reassign], envir = rlang::current_env())

  test$index(index, n = nrow(x))
  palette = get_pallete(palette, length(series))

  # Data:
  data <- data_ggvar_values(x, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(aes(color = .data$serie), ...) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(
      title = setup$title, x = "Index", y = "Values", color = "Serie"
    )
}
