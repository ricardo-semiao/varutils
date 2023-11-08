#' Plot VAR Residuals Distribution
#'
#' Plots the histogram of the residuals of a VAR model, or of the variables in a dataset, overlapped with a normal curve.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset (object coercible to data.frame) with numeric variables.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param plot_normal Logical, whether or not the normal curve should be plotted.
#' @param palette A vector of colors (bins, normal curve). See \code{vignette("palettes")}.
#' @param bins An integer. The number of histogram bins, passed to \link[ggplot2]{geom_histogram}.
#' @param size An integer. The size of the normal curve line, passed to \link[ggplot2]{geom_line}.
#' @param ... Additional arguments passed to \link[ggplot2]{geom_histogram}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_distribution(vars::VAR(EuStockMarkets))
#'
#' @export
ggvar_distribution = function(
    x, series = NULL, plot_normal = TRUE,
    palette = c("grey", "black"), bins = 30, size = 1, ...
  ) {
  # Initial tests:
  test$class_arg(x, c("data.frame", "matrix", "varest"))
  test$series(series, x)
  test$boolean_arg(plot_normal)

  # Create values:
  if (inherits(x, "varest")) {
    title <- "VAR Residuals Distribution"
    data <- as.data.frame(stats::residuals(x))
  } else {
    title <- "Time Series Distribution"
    data <- as.data.frame(x)
  }

  series <- series %||% colnames(data)
  palette <- get_pallete(palette, 2)

  # Data - residuals and density:
  data_resid <- dplyr::select(data, dplyr::all_of(series))

  data_histogram <- tidyr::pivot_longer(data_resid, dplyr::everything(), names_to = "serie", values_to = "residual")

  data_density <- purrr::imap_dfr(data_resid, function(col, varname){
    tibble::tibble(residual = seq(min(col), max(col), length = 200),
                   serie = varname,
                   density = stats::dnorm(residual, sd = stats::sd(col)))
  })

  # Graph:
  ggplot(data_histogram, aes(x = residual)) +
    ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)), fill = palette[1], bins = bins, ...) +
    ggplot2::geom_line(aes(y = density, color = "Normal curve"), data = data_density, size = size) +
    ggplot2::scale_color_manual(values = palette[2], name = "Legend") +
    ggplot2::facet_wrap(vars(.data$serie), ncol = 1, scales = "free") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = title, x = "Values", y = "Density")
}
