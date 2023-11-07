#' Plot for Structural Stability of a VAR
#'
#' Plots the result of a \link[vars]{stability} call. Confidence intevals are calculated using \link[strucchange]{boundary}.
#'
#' @param x A "varest" object to pass to \link[vars]{stability}, or, directly, a "varstabil" object.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param ci The level of confidence for the \link[strucchange]{boundary}.
#' @param ... Further arguments passed to \link[strucchange]{boundary}.
#' @param palette A vector of colors (line, conf. interval). See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_stability(vars::VAR(EuStockMarkets))
#'
#' @export
ggvar_stability <- function(
    x, series = NULL,
    ci = 0.95, ...,
    palette = c("black", "blue"), scales = "fixed", ncol = 1
  ) {
  # Initial tests:
  stopifnot(inherits(x, c("varest", "varstabil")))

  # Create values:
  series <- series %||% names(x$varresult)
  palette <- get_pallete(palette, 2)

  # Data - stability:
  stab <- if (inherits(x, "varest")) vars::stability(x)$stability else x$stability
  interval <- strucchange::boundary(stab[[1]], alpha = 1 - ci, alt.boundary = FALSE, functional = "max")

  data_stability <- purrr::imap_dfr(stab, function(x, name) {
    data.frame(equation = name,
               index = stats::time(x$process),
               value = as.numeric(x$process))
  })

  ggplot(data_stability, aes(index, value)) +
    ggplot2::geom_line(color = palette[1]) +
    ggplot2::geom_hline(yintercept = c(-interval, interval), color = palette[2], linetype = 2) +
    ggplot2::facet_wrap(vars(.data$equation), scales = scales, ncol = ncol) +
    ggplot2::labs(title = "VAR Structural Stability Analisys", x = "Index", y = "Values")
}
