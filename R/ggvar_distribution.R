#' @noRd
setup_ggvar_distribution <- function(x, series, plot_normal, palette) {
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
    palette = get_pallete(palette, 2),
    title = title
  )
}

#' Plot VAR Residuals Distribution
#'
#' Plots the histogram of the residuals of a VAR model, or of the variables in a dataset, overlapped with a normal curve.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset (object coercible to data.frame) with numeric variables.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param plot_normal Logical, whether or not the normal curve should be plotted.
#' @param palette A vector of colors (bins, normal curve). See \code{vignette("palettes")}.
#' @param bins An integer. The number of histogram bins, passed to \link[ggplot2]{geom_histogram}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An integer. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param linewidth An integer. The linewidth of the normal curve line, passed to \link[ggplot2]{geom_line}.
#' @param ... Additional arguments passed to \link[ggplot2]{geom_histogram}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_distribution(vars::VAR(freeny[-2]))
#'
#' @export
ggvar_distribution <- function(
    x, series = NULL, plot_normal = TRUE,
    palette = c("grey", "black"), bins = 30,
    linewidth = 1, scales = "fixed", ncol = 1, ...) {
  # Setup:
  setup <- setup_ggvar_distribution(x, series, plot_normal, palette)
  reassign <- c("x", "series", "palette")
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
    ggplot_add <- list(ggplot2::geom_line(aes(y = .data$density, color = "Normal curve"),
      data = data_density, linewidth = linewidth
    ))
  } else {
    ggplot_add <- list(NULL)
  }

  # Graph:
  ggplot(data_histogram, aes(x = .data$residual)) +
    ggplot2::geom_histogram(aes(y = ggplot2::after_stat(.data$density)),
      fill = palette[1], bins = bins, ...
    ) +
    ggplot_add +
    ggplot2::scale_color_manual(values = palette[2], name = "Legend") +
    ggplot2::facet_wrap(vars(.data$serie), ncol = ncol, scales = scales) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = setup$title, x = "Values", y = "Density")
}
