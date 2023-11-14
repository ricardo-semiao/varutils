#' @noRd
setup_ggvar_distribution <- function(x, series, plot_normal) {
  test$class_arg(x, c("data.frame", "matrix", "varest"))
  x <- test$dataset_arg(x)
  test$series(series, x)
  test$boolean_arg(plot_normal)

  if (inherits(x, "varest")) {
    title <- "VAR Residuals Distribution"
    x <- as.data.frame(stats::residuals(x))
  } else {
    title <- "Time Series Distribution"
    x <- as.data.frame(x)
  }

  list(
    x = x,
    series = series %||% get_names(x),
    title = title
  )
}

#' Plot VAR Residuals Distribution
#'
#' Plots the histogram of the residuals of a VAR model, or of the variables in a
#'  dataset, overlapped with a normal curve.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval param_series()
#' @param plot_normal Logical, whether or not the normal curve should be plotted.
#' @eval param_args(c("geom_histogram", "geom_line", "facet_wrap"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_distribution(vars::VAR(freeny[-2]))
#'
#' @export
ggvar_distribution <- function(
    x, series = NULL, plot_normal = TRUE,
    args_histogram = list(),
    args_line = list(),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_distribution(x, series, plot_normal)
  reassign <- c("x", "series")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  data_resid <- dplyr::select(x, dplyr::all_of(series))

  data_histogram <- tidyr::pivot_longer(data_resid, dplyr::everything(),
    names_to = "serie", values_to = "residual"
  )

  if (plot_normal) {
    data_density <- purrr::imap_dfr(data_resid, function(col, varname) {
      tibble::tibble(
        residual = seq(min(col), max(col), length = 200),
        serie = varname,
        density = stats::dnorm(.data$residual, sd = stats::sd(col))
      )
    })
    ggplot_add <- list(inject(ggplot2::geom_line(aes(y = .data$density),
      !!!args_line, data = data_density
    )))
  } else {
    ggplot_add <- list(NULL)
  }

  # Graph:
  ggplot(data_histogram, aes(x = .data$residual)) +
    inject(ggplot2::geom_histogram(aes(y = ggplot2::after_stat(.data$density)),
      !!!args_histogram
    )) +
    ggplot_add +
    inject(ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)) +
    ggplot2::labs(title = setup$title, x = "Values", y = "Density")
}
