#' Plot VAR Residuals Distribution
#'
#' Plots the histogram of the residuals of a VAR model, overlayed with a normal curve.
#'
#' @param x A "varest" object.
#' @param vars A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param bins An interger. The number of histogram bins, passed to \link[ggplot2]{geom_histogram}.
#' @param size An interger. The size of the normal curve line, passed to \link[ggplot2]{geom_line}.
#' @param palette A vector of colors (bins, normal curve). See `vignette(palettes)`.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' x <- vars::VAR(EuStockMarkets)
#' ggvar_dist(x)
#'
#' @export
ggvar_dist = function(
    x, vars = NULL,
    bins = 30, size = 1, palette = c("grey", "black")
  ) {
  # Initial tests:
  stopifnot(inherits(x, "varest"), inherits(vars, c("character", "NULL")))

  # Default values:
  vars <- vars %||% names(x$varresult)
  palette <- get_pallete(palette, length(vars))

  # Data - residuals and density:
  data_resid <- as.data.frame(residuals(x))

  data_histogram <- tidyr::pivot_longer(data_resid, dplyr::everything(), names_to = "Variable", values_to = "Resituals")

  data_density <- purrr::imap_dfr(data_resid, function(col, varname){
    tibble::tibble(Resituals = seq(min(col), max(col), length = 200),
                   Variable = varname,
                   Density = dnorm(Resituals, sd = sd(col)))
  })

  # Graph:
  ggplot(data_histogram, aes(x = Resituals)) +
    ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)), fill = palette[1], bins = bins) +
    ggplot2::geom_line(aes(y = Density, color = "Normal curve"), data = data_density, size = size) +
    ggplot2::scale_color_manual(values = palette[2], name = "Legend") +
    ggplot2::facet_wrap(ggplot2::vars(.data$Variable), ncol = 1, scales = "free") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = "VAR Residuals Distribution", y = "Density")
}
