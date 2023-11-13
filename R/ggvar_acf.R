#' @noRd
setup_ggvar_acf <- function(
    x, series, ci, type, lag.max,
    geom, palette, facet = NULL) {
  x <- test$dataset_arg(x)
  test$class_arg(x, c("data.frame", "matrix", "varest"))
  test$series(series, x)
  test$categorical_arg(geom, c("segment", "area"))
  test$interval_arg(ci, 0, 1, FALSE)
  if (!is.null(facet)) test$categorical_arg(facet, c("ggplot", "ggh4x"))

  if (inherits(x, c("varest"))) {
    title_add <- "Residuals"
    x <- as.data.frame(stats::residuals(x))
  } else {
    title_add <- "Series"
    x <- as.data.frame(x)
  }

  lag.max <- lag.max %||% ceiling(10 * log(nrow(x) / ncol(x), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  list(
    x = x,
    series = series %||% get_names(x),
    palette = get_pallete(palette, 4),
    title_add = title_add,
    lag.max = lag.max,
    lag.min = lag.min
  )
}

#' Plot Autocorrelation (and Similars) of Dataset
#'
#' \code{ggvar_acf} plots the result of a \link[stats]{acf} call for every series, using ggplot and \link[ggplot2]{facet_wrap}. \code{ggvar_ccf} plots all the cross correlations (and similars) between the series, using ggplot in a matrix fashion with \link[ggplot2]{facet_grid}.
#'
#' @param x A dataset (object coercible to data.frame) or a "varest" object to get residuals from.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param type The type of ACF to be computed, passed to \link[stats]{acf}. Can be either "correlation", "covariance", or "partial".
#' @param lag.max The number of lags used to calculate the ACF, passed to \link[stats]{acf}. defaults to \code{ceiling(10 * log(nrow(data) / ncol(data), base = 10))}.
#' @param ci The level of confidence for the ACF confidence interval. Set to \code{FALSE} to omit.
#' @param geom The ggplot geom used to create the plot, "segment" for \link[ggplot2]{geom_segment} (the default) or "area" for \link[ggplot2]{geom_area}.
#' @param facet The facet "engine" to be used. "ggplot2" for \link[ggplot2]{facet_grid}, "ggh4x" for \link[ggh4x]{facet_grid2}.
#' @param palette A vector of colors (bins, normal curve). See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An integer. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param independent For varying the scales of each cell. See \link[ggh4x]{facet_grid2}.
#' @param alpha A double. The alpha aesthetic for the points, passed to \link[ggplot2]{geom_ribbon}.
#' @param ... Additional arguments passed to the ggplot geom defined by \code{geom}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_acf(freeny[-2], scales = "free_y")
#' ggvar_ccf(freeny[-2], scales = "free_y")
#' ggvar_acf(vars::VAR(freeny[-2]), scales = "free_y")
#'
#' @export
ggvar_acf <- function(
    x, series = NULL,
    type = "correlation", lag.max = NULL, ci = 0.95,
    geom = "segment", palette = c("black", "black", "blue", NA),
    scales = "fixed", ncol = 1, alpha = 0.5, ...) {
  # Setup:
  setup <- setup_ggvar_acf(x, series, ci, type, lag.max, geom, palette)
  reassign <- c("x", "series", "ci", "geom", "palette", "lag.max")
  list2env(setup[reassign], envir = rlang::current_env())

  title <- switch(type,
    "correlation" = paste("Auto-correlation of", setup$title_add),
    "covariance" = paste("Auto-covariance of", setup$title_add),
    "partial" = paste("Auto-partial-correlation of", setup$title_add)
  )

  # Data:
  data <- x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    purrr::map2_dfr(series, function(col, name) {
      tibble::tibble(
        serie = name,
        value = stats::acf(col, lag.max = lag.max, type = type, plot = FALSE) %>%
          purrr::pluck("acf") %>%
          `[`(, , 1),
        lag = setup$lag.min:lag.max
      )
    })

  # Graph:
  ggplot_add <- list(
    switch(geom,
      "segment" = ggplot2::geom_segment(aes(xend = .data$lag, yend = 0),
        color = palette[1], ...
      ),
      "area" = ggplot2::geom_area(aes(y = .data$value),
        fill = palette[1], ...
      )
    ),
    if (!isFALSE(ci)) {
      interval <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(x))
      ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval),
        linetype = 2,
        color = palette[3], fill = palette[4], alpha = alpha
      )
    }
  )

  ggplot(data, aes(.data$lag, .data$value)) +
    ggplot_add +
    ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
    ggplot2::facet_wrap(vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::labs(title = title[type], x = "Lags", y = "Values")
}

#' @rdname ggvar_acf
#' @export
ggvar_ccf <- function(
    x, series = NULL,
    type = "correlation", lag.max = NULL, ci = 0.95,
    geom = "segment", facet = "ggplot", palette = c("black", "black", "blue", NA),
    scales = "fixed", independent = "none", alpha = 0.5, ...) {
  # Setup:
  setup <- setup_ggvar_acf(x, series, ci, type, lag.max, geom, palette, facet)
  reassign <- c("x", "series", "ci", "geom", "palette", "lag.max")
  list2env(setup[reassign], envir = rlang::current_env())

  title <- switch(type,
    "correlation" = paste("Cross-correlation of", setup$title_add),
    "covariance" = paste("Cross-covariance of", setup$title_add),
    "partial" = paste("Cross-partial-correlation of", setup$title_add)
  )

  # Data:
  data <- x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    stats::acf(lag.max = lag.max, type = type, plot = FALSE) %>%
    purrr::pluck("acf") %>%
    purrr::array_tree(3) %>%
    purrr::map2_dfr(series, ~ data.frame(.y, setup$lag.min:lag.max, .x)) %>%
    stats::setNames(c("var_row", "lag", series)) %>%
    tidyr::pivot_longer(dplyr::all_of(series),
      names_to = "var_col",
      values_to = "value"
    )

  # Graph:
  ggplot_add <- list(
    switch(geom,
      "segment" = ggplot2::geom_segment(aes(xend = .data$lag, yend = 0),
        color = palette[1], ...
      ),
      "area" = ggplot2::geom_area(aes(y = .data$value),
        fill = palette[1], ...
      )
    ),
    if (!isFALSE(ci)) {
      interval <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(x))
      ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval),
        linetype = 2,
        color = palette[3], fill = palette[4], alpha = alpha
      )
    },
    define_facet(facet, "var_row", "var_col", scales, independent)
  )

  ggplot(data, aes(.data$lag, .data$value)) +
    ggplot_add +
    ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
    ggplot2::labs(title = title, x = "Lags", y = "Values")
}
