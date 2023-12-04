#' @noRd
setup_ggvar_values <- function(x, series, index) {
  x <- test$dataset_arg(x)
  test$class_arg(x, c("data.frame", "matrix", "varest"))
  test$series(series, x)

  # Create values:
  if (inherits(x, "varest")) {
    title <- "VAR Residuals Historic Values"
    index <- index %||% (x$p + 1):x$totobs
    x <- as.data.frame(stats::residuals(x))

  } else {
    title <- "Time Series Historic Values"
    x <- as.data.frame(x)
    index <- index %||% seq_len(nrow(x))
  }

  list(
    x = x,
    series = series %||% get_names(x),
    index = index,
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
#' Plots the historic values of variables in a dataset, or residuals of a VAR
#'  model. \code{ggvar_values} Plots each series in a facet.
#'  \code{ggvar_values_colored} plots all in the same graph, each with a
#'  different color.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval param_series()
#' @eval param_index("\\code{x$obs} or \\code{nrow(x)}")
#' @eval param_colors()
#' @eval param_args(c("geom_line", "facet_wrap"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_values(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_values_colored(freeny[-2])
#' ggvar_values(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_values <- function(
    x, series = NULL, index = NULL,
    args_line = list(),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_values(x, series, index)
  reassign <- c("x", "series", "index")
  list2env(setup[reassign], envir = rlang::current_env())

  test$index(index, n = nrow(x), "'obs' of `x` in varest form")

  # Data:
  data <- data_ggvar_values(x, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    inject(ggplot2::geom_line(!!!args_line)) +
    inject(ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)) +
    ggplot2::labs(title = setup$title, x = "Index", y = "Values")
}

#' @rdname ggvar_values
#' @export
ggvar_values_colored <- function(
    x, series = NULL, index = NULL,
    colors = NULL,
    args_line = list()) {
  # Setup:
  setup <- setup_ggvar_values(x, series, index)
  reassign <- c("x", "series", "index")
  list2env(setup[reassign], envir = rlang::current_env())

  test$index(index, n = nrow(x))
  colors <- get_pallete(colors, length(series))

  # Data:
  data <- data_ggvar_values(x, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    inject(ggplot2::geom_line(aes(color = .data$serie), !!!args_line)) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = setup$title, x = "Index", y = "Values", color = "Series"
    )
}
