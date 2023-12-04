#' @noRd
setup_ggvar_acf <- function(x, series, ci, type, lag.max, geom, facet = NULL) {
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
    title_add = title_add,
    lag.max = lag.max,
    lag.min = lag.min
  )
}

#' @noRd
data_ggvar_ccf <- function(x, serie_x, serie_y, ...) {
  ci <- qnorm((1 - 0.95) / 2) / sqrt(nrow(x))
  temp_ccf <- ccf(x[[serie_x]], x[[serie_y]], plot = FALSE, ...)

  tibble::tibble(
    series = paste0(serie_x, " - ", serie_y),
    lag = temp_ccf$lag[,,1],
    value = temp_ccf$acf[,,1],
    ci = ci
  )
}


#' Plot autocorrelation (and similars) of dataset
#'
#' \code{ggvar_acf} plots the result of a \link[stats]{acf} call for every
#'  series, using \link[ggplot2]{facet_wrap}. \code{ggvar_ccf} plots all the
#'  cross correlations (and similars) between the series, using
#'  \link[ggplot2]{facet_grid}.
#'
#' @param x A dataset (object coercible to data.frame) or a "varest" object to
#'  get residuals from.
#' @eval param_series()
#' @param type The type of ACF to be computed, passed to \link[stats]{acf}. Can
#'  be either "correlation", "covariance", or "partial".
#' @param lag.max The number of lags used to calculate the ACF, passed to
#'  \link[stats]{acf}. defaults to \code{10 * log(nrow(x) / ncol(x), base = 10)}.
#' @param ci The level of confidence for the ACF confidence interval. Set to
#'  \code{FALSE} to omit the \link[ggplot2]{geom_ribbon}.
#' @eval param_dots("stats::acf")
#' @eval param_geom(c("geom_segment", "geom_area"))
#' @eval param_facet()
#' @eval param_args_geom()
#' @eval param_args(c("geom_ribbon", "geom_hline", "facet_wrap"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_acf(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_ccf(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_acf(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_acf <- function(
    x, series = NULL,
    type = "correlation", lag.max = NULL, ci = 0.95, ...,
    geom = "segment",
    args_geom = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_acf(x, series, ci, type, lag.max, geom)
  reassign <- c("x", "series", "ci", "geom", "lag.max")
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
        value = stats::acf(col, ...,
         lag.max = lag.max, type = type, plot = FALSE
        ) %>%
          purrr::pluck("acf") %>%
          `[`(, , 1),
        lag = setup$lag.min:lag.max
      )
    })

  # Graph:
  ggplot_add <- list(
    switch(geom,
      "segment" = inject(ggplot2::geom_segment(aes(xend = .data$lag, yend = 0),
                   !!!args_geom
                  )),
      "area" = inject(ggplot2::geom_area(aes(y = .data$value),
                 !!!args_geom
                ))
    ),
    if (!isFALSE(ci)) {
      interval <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(x))
      inject(ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval),
       !!!args_ribbon
      ))
    }
  )

  ggplot(data, aes(.data$lag, .data$value)) +
    ggplot_add +
    inject(ggplot2::geom_hline(yintercept = 0, !!!args_hline)) +
    inject(ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)) +
    ggplot2::labs(title = title, x = "Lags", y = "Values")
}

#' @rdname ggvar_acf
#' @export
ggvar_ccf_grid <- function(
    x, series = NULL,
    type = "correlation", lag.max = NULL, ci = 0.95, ...,
    geom = "segment", facet = "ggplot",
    args_geom = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_acf(x, series, ci, type, lag.max, geom, facet)
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
    stats::acf(lag.max = lag.max, type = type, plot = FALSE, ...) %>%
    purrr::pluck("acf") %>%
    purrr::array_tree(3) %>%
    purrr::map2_dfr(series, ~ data.frame(.y, setup$lag.min:lag.max, .x)) %>%
    purrr::set_names(c("var_row", "lag", series)) %>%
    tidyr::pivot_longer(dplyr::all_of(series),
      names_to = "var_col",
      values_to = "value"
    )

  # Graph:
  ggplot_add <- list(
    switch(geom,
      "segment" = inject(ggplot2::geom_segment(aes(xend = .data$lag, yend = 0),
                   !!!args_geom
                  )),
      "area" = inject(ggplot2::geom_area(aes(y = .data$value),
                 !!!args_geom
                ))
    ),
    if (!isFALSE(ci)) {
      interval <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(x))
      inject(ggplot2::geom_ribbon(aes(ymin = -interval, ymax = interval),
       !!!args_ribbon
      ))
    },
    inject(define_facet(facet, "var_row", "var_col", !!!args_facet))
  )

  ggplot(data, aes(.data$lag, .data$value)) +
    ggplot_add +
    inject(ggplot2::geom_hline(yintercept = 0, !!!args_hline)) +
    ggplot2::labs(title = title, x = "Lags", y = "Values")
}


#' @rdname ggvar_acf
#' @export
ggvar_ccf_ind <- function(x, serie_x, serie_y, ...) {
  data <- data_ggvar_ccf(x, serie_x, serie_y, ...)

  ggplot(data, aes(x = lag, y = value)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_segment(aes(xend = lag, yend = 0)) +
    ggplot2::geom_ribbon(aes(ymin = -ci, ymax = ci), fill = NA) +
    ggplot2::labs(
      title = paste("Cross correlation of", serie_x, "and", serie_y),
      y = "Value", x = "Lag"
    )
}


#' @rdname ggvar_acf
#' @export
ggvar_ccf_wrap <- function(x, cols, ...) {
  combs <- asplit(combn(cols, 2), 2)
  data <- purrr::map_dfr(combs, \(l) data_ggvar_ccf(x, l[1], l[2]), ...)

  ggplot(data, aes(x = lag, y = value)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_segment(aes(xend = lag, yend = 0)) +
    ggplot2::geom_ribbon(aes(ymin = -ci, ymax = ci), fill = NA) +
    ggplot2::facet_wrap(vars(series)) +
    ggplot2::labs(y = "Cross correlation of variables", x = "Lag")
}
