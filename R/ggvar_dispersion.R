#' @noRd
setup_ggvar_dispersion <- function(x, series) {
  test$class_arg(x, c("varest"))
  test$series(series, x)

  list(
    series = series %||% get_names(x)
  )
}

#' Plot VAR Residuals Dispersion
#'
#' Plots a scatterplot of the residuals versus fitted values of a VAR model,
#'  using ggplot2.
#'
#' @param x A "varest" object to get residuals and fitted values from.
#' @eval param_series()
#' @eval param_args(c("geom_point", "geom_hline", "facet_wrap"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_dispersion(vars::VAR(freeny[-2]), args_facet = list(scales = "free_x"))
#'
#' @export
ggvar_dispersion <- function(
    x, series = NULL,
    args_point = list(),
    args_hline = list(),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_dispersion(x, series)
  reassign <- c("series")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  data <- data.frame(
    residual.. = stats::residuals(x),
    fitted.. = stats::fitted(x)
  ) %>%
    dplyr::select(dplyr::ends_with(series)) %>%
    tidyr::pivot_longer(dplyr::everything(),
      names_sep = "\\.\\.\\.",
      names_to = c(".value", "serie")
    )

  # Graph:
  ggplot(data, aes(.data$fitted, .data$residual)) +
    inject(ggplot2::geom_point(!!!args_point)) +
    inject(ggplot2::geom_hline(yintercept = 0, !!!args_hline)) +
    inject(ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)) +
    ggplot2::labs(
      title = "VAR Residuals Dispersion", x = "Fitted", y = "Residuals"
    )
}
