#' Plot VAR Residuals Dispersion
#'
#' Plots a scatterplot of the residuals versus fitted values of a VAR model, using ggplot2.
#'
#' @param x A "varest" object to get residuals and fitted values from.
#' @param ncol A interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param alpha A double. The alpha aesthetic for the points, passed to \link[ggplot2]{geom_point}.
#' @param palette A vector of colors. Each for a different ggplot2 element, as denoted by the (optional) names.
#'
#' @return Returns an object of class \code{ggplot}.
#'
#' @seealso For info on \code{alpha} and \code{colors}: \code{vignette("ggplot2-specs")}.
#'
#' @export
#'
#' @examples
#' x <- VAR(EuStockMarkets)
#' ggdispersion(x)
ggdispersion <- function(x, ncol = 1, alpha = 0.5, palette = c(points = "black", xaxis = "black")){
  # Initial tests:
  stopifnot(inherits(x, "varest"))
  if (ncol %% 1 != 0)  ncol <- trunc(ncol); warning("Decimal `ncol` was truncated")
  if (alpha < 0 | alpha > 1) x <- max(0, min(1, x)); warning("`alpha` outside [0,1], coerced to interval")
  if (is.list(palette)) palette <- unlist(palette); warning("`palette` was a list, converted to a atomic vector")

  # Getting Residuals and fitted values
  data <- data.frame(Residuals.. = stats::residuals(x), Fitted.. = vars::fitted(x)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_sep = "\\.\\.\\.", names_to = c(".value", "Variable"))

  ggplot(data, aes(.data$Fitted, .data$Residuals)) +
    ggplot2::geom_point(color = palette[1], alpha = alpha) +
    ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Variable), scales = "free", ncol = ncol) +
    ggplot2::labs(title = "VAR Residuals Dispersion")
}
