#' Plot VAR Residuals Dispersion
#'
#' Plots a scatterplot of the residuals versus fitted values of a VAR model, using ggplot2.
#'
#' @param x A "varest" object to get residuals and fitted values from.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param palette A vector of colors (points, x-axis line). See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param alpha A double. The alpha aesthetic for the points, passed to \link[ggplot2]{geom_point}.
#' @param ... Aditional arguments passed to \link[ggplot2]{geom_point}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @export
#'
#' @examples
#' ggvar_dispersion(vars::VAR(EuStockMarkets))
ggvar_dispersion <- function(
    x, series = NULL,
    palette = c("black", "black"), scales = "fixed", ncol = 1, alpha = 0.5, ...
  ) {
  # Initial tests:
  stopifnot(inherits(x, "varest"), inherits(series, c("character", "NULL")))

  # Create values:
  palette <- get_pallete(palette, 2)
  series <- series %||% names(x$varresult)

  # Data - fitted and residuals:
  data <- data.frame(residual.. = stats::residuals(x), fitted.. = stats::fitted(x)) %>%
    dplyr::select(dplyr::ends_with(series)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_sep = "\\.\\.\\.", names_to = c(".value", "serie"))

  # Graph:
  ggplot(data, aes(.data$fitted, .data$residual)) +
    ggplot2::geom_point(color = palette[1], alpha = alpha, ...) +
    ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
    ggplot2::facet_wrap(ggplot2::vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = "VAR Residuals Dispersion")
}