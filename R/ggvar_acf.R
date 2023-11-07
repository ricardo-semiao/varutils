#' Plot Autocorrelation (and Similars) of Dataset
#'
#' \code{ggvar_acf} plots the result of a \link[stats]{acf} call for every series, using ggplot and \link[ggplot2]{facet_wrap}. \code{ggvar_ccf} plots all the cross correlations (and similars) between the series, using ggplot in a matrix fashion with \link[ggplot2]{facet_grid}.
#'
#' @param data A dataset (object coercible to data.frame) with variables to calculate de ACF
#' @param type The type of ACF to be computed, passed to \link[stats]{acf}. Can be either "correlation", "covariance", or "partial".
#' @param lag.max The number of lags used to calculate the ACF, passed to \link[stats]{acf}. defaults to \code{ceiling(10 * log(nrow(data) / ncol(data), base = 10))}.
#' @param ci The level of confidence for the ACF confidence interval.
#' @param geom The ggplot geom used to create the plot, "segment" for \link[ggplot2]{geom_segment} (the default) or "area" for \link[ggplot2]{geom_area}.
#' @param facet The facet "engine" to be used. "ggplot2" for \link[ggplot2]{facet_grid}, "ggh4x" for \link[ggh4x]{facet_grid2}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param palette A vector of colors (bins, normal curve). See \code{vignette("palettes")}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param independent If scales are able to vary between rows and/or columns. See \link[ggh4x]{facet_grid2}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_acf(EuStockMarkets)
#' ggvar_ccf(EuStockMarkets)
#'
#' @export
ggvar_acf <- function(
    data, type = "correlation",
    lag.max = NULL, ci = 0.95,
    geom = "segment", facet = "ggplot",
    palette = c("black", "black", "blue", NA), scales = "fixed", ncol = 1
  ){
  # Initial tests:
  stopifnot(inherits(data, c("data.frame", "matrix")))

  # Create values:
  series <- colnames(data)
  palette <- get_pallete(palette, 4)
  title <- c("correlation" = "Auto-correlation of Series",
             "covariance" = "Auto-covariance of Series",
             "partial" = "Auto-partial-correlation of Series")

  interval <- stats::qnorm((1 - ci)/2) / sqrt(nrow(data))
  lag.max <- lag.max %||% ceiling(10 * log(nrow(data) / ncol(data), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  ggplot_add <- list(
    if (geom == "area") {
      ggplot2::geom_area(aes(ymin = 0, ymax = .data$values), fill = palette[1])
    } else {
      ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), color = palette[1])
    }
  )

  # Data
  data_acf <- purrr::map2_dfr(as.list(data), series, function(x, name) {
    tibble::tibble(
      serie = name,
      value = stats::acf(x, lag.max = lag.max, type = type, plot = FALSE) %>% purrr::pluck("acf") %>% `[`(,,1),
      lag = lag.min:lag.max
    )
  })

  ggplot(data_acf, aes(.data$lag, .data$value)) +
    ggplot_add +
    ggplot2::geom_hline(yintercept = 0, color = palette[3]) +
    ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval), linetype = 2, color = palette[2], fill = palette[4]) +
    ggplot2::facet_wrap(ggplot2::vars(serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = ifelse(type == "partial", "Partial Autocorrelation", paste0("Auto", type)))
}

#' @rdname ggvar_acf
#' @export
ggvar_ccf <- function(
    data, type = "correlation",
    lag.max = NULL, ci = 0.95,
    facet = "ggplot", geom = "segment",
    palette = c("black", "black", "blue", NA), scales = "fixed", independent = "y"
){
  # Initial tests:
  stopifnot(inherits(data, c("data.frame", "matrix")))

  # Create values:
  vars <- colnames(data)
  palette <- get_pallete(palette, 4)
  title <- c("correlation" = "Cross-correlation Between Series",
             "covariance" = "Cross-covariance Between Series",
             "partial" = "Cross-partial-correlation Between Series")

  interval <- stats::qnorm((1 - ci)/2) / sqrt(nrow(data))
  lag.max <- lag.max %||% ceiling(10 * log(nrow(data) / ncol(data), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  ggplot_add <- list(
    if (geom == "area") {
      ggplot2::geom_area(aes(ymin = 0, ymax = .data$value), fill = palette[1])
    } else if (geom == "segment") {
      ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), color = palette[1])
    } else {stop("Invalid `geom` argument.")},
    if (facet == "ggh4x") {
      if (!rlang::is_installed("ggh4x")) {
        warning("Package ggh4x is not installed. Coercing `facet = 'ggplot'`.")
        facet <- "ggplot"
      } else {
        ggh4x::facet_grid2(var1 ~ var2, scales = scales, independent = independent)
      }
    } else if (facet == "ggplot") {
      ggplot2::facet_grid(var1 ~ var2, scales = scales)
    } else {stop("Invalid `facet` argument.")}
  )

  # Data
  data_acf <- stats::acf(data, lag.max = lag.max, type = type, plot = FALSE) %>%
    purrr::pluck("acf") %>%
    purrr::array_tree(3) %>%
    purrr::map2_dfr(vars, ~ data.frame(.y, lag.min:lag.max, .x)) %>%
    utils::setNames(c("var1", "lag", vars)) %>%
    tidyr::pivot_longer(dplyr::all_of(vars), names_to = "var2", values_to = "value")

  ggplot(data_acf, aes(.data$lag, .data$value)) +
    ggplot_add +
    ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
    ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval), linetype = 2, color = palette[3], fill = palette[4]) +
    ggplot2::labs(title = title[type])
}
