#' Plot Autocorrelation (and Similars) of Dataset
#'
#' \code{ggvar_acf} plots the result of a \link[stats]{acf} call for every series, using ggplot and \link[ggplot2]{facet_wrap}. \code{ggvar_ccf} plots all the cross correlations (and similars) between the series, using ggplot in a matrix fashion with \link[ggplot2]{facet_grid}.
#'
#' @param x A dataset (object coercible to data.frame) or a "varest" object to get residuals from.
#' @param type The type of ACF to be computed, passed to \link[stats]{acf}. Can be either "correlation", "covariance", or "partial".
#' @param lag.max The number of lags used to calculate the ACF, passed to \link[stats]{acf}. defaults to \code{ceiling(10 * log(nrow(data) / ncol(data), base = 10))}.
#' @param ci The level of confidence for the ACF confidence interval.
#' @param geom The ggplot geom used to create the plot, "segment" for \link[ggplot2]{geom_segment} (the default) or "area" for \link[ggplot2]{geom_area}.
#' @param facet The facet "engine" to be used. "ggplot2" for \link[ggplot2]{facet_grid}, "ggh4x" for \link[ggh4x]{facet_grid2}.
#' @param palette A vector of colors (bins, normal curve). See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param independent If scales are able to vary between rows and/or columns. See \link[ggh4x]{facet_grid2}.
#' @param ... Aditional arguments passed to the ggplot geom defined by \code{geom}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_acf(EuStockMarkets)
#' ggvar_ccf(EuStockMarkets)
#' ggvar_acf(vars::VAR(EuStockMarkets))
#'
#' @export
ggvar_acf <- function(
    x, type = "correlation",
    lag.max = NULL, ci = 0.95,
    geom = "segment", facet = "ggplot",
    palette = c("black", "black", "blue", NA), scales = "fixed", ncol = 1, ...
  ){
  # Initial tests:
  stopifnot(inherits(x, c("data.frame", "matrix", "varest")))

  # Create values:
  if (inherits(x, c("varest"))) {
    title_add <- "Residuals"
    data <- as.data.frame(stats::residuals(x))
  } else {
    title_add <- "Series"
    data <- as.data.frame(x)
  }

  series <- colnames(data)
  palette <- get_pallete(palette, 4)
  title <- c("correlation" = paste("Auto-correlation of", title_add),
             "covariance" = paste("Auto-covariance of", title_add),
             "partial" = paste("Auto-partial-correlation of", title_add))

  interval <- stats::qnorm((1 - ci)/2) / sqrt(nrow(data))
  lag.max <- lag.max %||% ceiling(10 * log(nrow(data) / ncol(data), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  ggplot_add <-  switch(
    geom,
    "segment" = list(ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), color = palette[1], ...)),
    "area" = list(ggplot2::geom_area(aes(ymin = 0, ymax = .data$values), fill = palette[1], ...)),
    stop("Invalid `geom` argument. Choose 'segment' or  'area'")
  )

  # Data
  data_acf <- purrr::map2_dfr(as.data.frame(data), series, function(x, name) {
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
    x, type = "correlation",
    lag.max = NULL, ci = 0.95,
    facet = "ggplot", geom = "segment",
    palette = c("black", "black", "blue", NA), scales = "fixed", independent = "y", ...
){
  # Initial tests:
  stopifnot(inherits(x, c("data.frame", "matrix", "varest")))

  # Create values:
  if (inherits(x, "varest")) {
    title_add <- "Residuals"
    data <- as.data.frame(stats::residuals(x))
  } else {
    title_add <- "Series"
    data <- as.data.frame(x)
  }

  series <- colnames(data)
  palette <- get_pallete(palette, 4)
  title <- c("correlation" = paste("Cross-correlation of", title_add),
             "covariance" = paste("Cross-covariance of", title_add),
             "partial" = paste("Cross-partial-correlation of", title_add))

  interval <- stats::qnorm((1 - ci)/2) / sqrt(nrow(data))
  lag.max <- lag.max %||% ceiling(10 * log(nrow(data) / ncol(data), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  ggplot_add <- list(
    if (geom == "area") {
      ggplot2::geom_area(aes(ymin = 0, ymax = .data$value), fill = palette[1], ...)
    } else if (geom == "segment") {
      ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), color = palette[1], ...)
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
    purrr::map2_dfr(series, ~ data.frame(.y, lag.min:lag.max, .x)) %>%
    stats::setNames(c("var1", "lag", series)) %>%
    tidyr::pivot_longer(dplyr::all_of(series), names_to = "var2", values_to = "value")

  ggplot(data_acf, aes(.data$lag, .data$value)) +
    ggplot_add +
    ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
    ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval), linetype = 2, color = palette[3], fill = palette[4]) +
    ggplot2::labs(title = title[type], x = "Lags", y = "Values")
}
