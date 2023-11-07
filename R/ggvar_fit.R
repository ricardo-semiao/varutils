#' Plot VAR Fitted Values
#'
#' Plots fitted values of a VAR model, versus the actual values. \code{ggvar_fit} Plots each series in a facet. \code{ggvar_fit_colored} plots all in the same graph, each with a different color.
#'
#' @param x A "varest" object to get fitted values from.
#' @param compare Logical, should the true values be printed?
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param index A vector of labels to the x-axis, normally dates. Must have length equal to x$obs. Defaults to a vector of indexes.
#' @param linetypes A vecotr of linetypes (fitted, original), passed to \link[ggplot2]{scale_linetype_manual}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param palette A vector of colors. Each for a different variable. See \code{vignette("palettes")}.
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
    x, compare = TRUE, series = NULL, index = 1:x$obs,
    linetypes = c("solid", "dashed"), scales = "fixed", ncol = 1
  ) {
  # Initial tests:
  stopifnot(inherits(x, "varest"), inherits(series, c("character", "NULL")))

  # Create values:
  series <- series %||% names(x$varresult)

  # Data - fitted and original values:
  data <- data.frame(index = index[(x$p + 1):x$totobs], Fitted.. = stats::fitted(x)) %>%
    `if`(compare, cbind(., Original.. = x$datamat[1:x$K]), .) %>%
    dplyr::select(c(index, dplyr::ends_with(series))) %>%
    tidyr::pivot_longer(-index, names_to = c("type", "serie"), names_sep = "\\.\\.\\.",  values_to = "value")

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(aes(linetype = .data$type)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = "Fitted VAR Values") +
    ggplot2::scale_linetype_manual(values = linetypes, guide = if (compare) "legend" else "none")
}

#' @rdname ggvar_fit
#' @export
ggvar_fit_colored <- function(
    x, series = NULL, index = 1:x$obs, compare = TRUE,
    palette = NULL, linetypes = c("solid", "dashed")
  ) {
  # Initial tests:
  stopifnot(inherits(x, "varest"), inherits(series, c("character", "NULL")))

  # Default values:
  series <- series %||% names(x$varresult)
  palette <- get_pallete(palette, length(series))

  # Data - fitted and original values:
  data <- data.frame(index = index[(x$p + 1):x$totobs], Fitted.. = stats::fitted(x)) %>%
    `if`(compare, cbind(., Original.. = x$datamat[1:x$K]), .) %>%
    dplyr::select(c(index, dplyr::ends_with(series))) %>%
    tidyr::pivot_longer(-index, names_to = c("type", "serie"), names_sep = "\\.\\.\\.",  values_to = "value")

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(aes(color = .data$serie, linetype = .data$type)) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(title = "Fitted VAR Values") +
    ggplot2::scale_linetype_manual(values = linetypes, guide = if (compare) "legend" else "none")
}
