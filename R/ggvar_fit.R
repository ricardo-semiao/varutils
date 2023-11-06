#' Plot VAR Fitted Values
#'
#' Plots fitted values of a VAR model, versus the actual values.
#'
#' @param x A "varest" object to get fitted values from.
#' @param vars A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param compare Logical, should the true values be printed?
#' @param date A vector of labels to the x-axis, normally dates. Must have length equal to x$obs. Defaults to a vector of indexes.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol A interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param alpha A double. The alpha aesthetic for the points, passed to \link[ggplot2]{geom_point}.
#' @param palette A vector of colors. Each for a different variable.
#' @param linetypes A vecotr of linetypes (fitted, original), passed to \link[ggplot2]{scale_linetype_manual}.
#'
#' @return A ggplot.
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap vars scale_color_manual labs
#'
#' @export
ggvar_fit = function(
    x,
    vars = NULL,
    date = 1:x$obs,
    compare = TRUE,
    scales = "fixed",
    ncol = 1,
    alpha = 0.5,
    palette = NULL,
    linetypes = c("solid", "dashed")
  ) {

  # Initial tests:
  stopifnot(inherits(x, "varest"), inherits(vars, c("character", "NULL")))

  # Default values:
  vars <- vars %||% names(x$varresult)
  palette <- get_pallete(palette, length(vars))


  data <- data.frame(Date = date, Fitted.. = stats::fitted(x)) %>%
    `if`(compare, cbind(., Original.. = x$datamat[1:x$K]), .) %>%
    dplyr::select(c(Date, dplyr::ends_with(vars))) %>%
    tidyr::pivot_longer(-Date, names_to = c("Serie", "Variable"),
                        names_sep = "\\.\\.\\.",  values_to = "Value")

  ggplot(data, aes(.data$Date, .data$Value)) +
    ggplot2::geom_line(aes(color = .data$Variable, linetype = .data$Serie)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Variable), scales = scales, ncol = ncol) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(title = "Fitted VAR Values") +
    ggplot2::scale_linetype_manual(values = linetypes, guide = if (compare) "legend" else "none")
}

#facet_grid(.data$Variable ~ .data$Serie, scales = "free_y")
#ggh4x::facet_grid2(.data$Variable ~ .data$Serie, scales = "free_y", independent = "y")
