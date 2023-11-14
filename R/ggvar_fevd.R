#' @noRd
setup_ggvar_fevd <- function(x, n.ahead, series, geom, colors) {
  test$class_arg(x, c("varfevd", "varest"))
  test$series(series, x)
  test$categorical_arg(geom, c("bar", "area", "line"))

  list(
    series = series %||% get_names(x),
    colors = get_pallete(colors, length(get_names(x))),
    n.ahead = n.ahead
  )
}

#' Plot for Forecast Error Variance Decomposition of a VAR
#'
#' Plots the result of a \link[vars]{fevd} call.
#'
#' @param x A "varest" object to pass to \link[vars]{fevd}, or, directly, a
#'  "varfevd" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to
#'  \link[vars]{fevd}. Unused if `x` is a "varfevd" object.
#' @eval param_series()
#' @eval param_geom(c("geom_segment", "geom_area", "geom_line"))
#' @eval param_colors()
#' @eval param_args_geom()
#' @eval param_args(c("facet_wrap"))
#' @param ... If \code{geom="line"}, additional arguments to
#'  \link[ggplot2]{geom_point}
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_fevd(vars::VAR(freeny[-2]), n.ahead = 10)
#'
#' @export
ggvar_fevd <- function(
    x, n.ahead = NULL, series = NULL,
    geom = "bar", colors = NULL,
    args_geom = list(),
    args_facet = list(),
    ...) {
  # Setup:
  setup <- setup_ggvar_fevd(x, n.ahead, series, geom, colors)
  reassign <- c("n.ahead", "series", "colors")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  fevd <- if (inherits(x, "varest")) vars::fevd(x, n.ahead) else x

  data <- fevd %>%
    purrr::imap_dfr(~ as.data.frame(.x) %>%
      dplyr::mutate(equation = .y, lead = seq_len(nrow(.)))) %>%
    tidyr::pivot_longer(-c("equation", "lead"),
      names_to = "serie", values_to = "value"
    ) %>%
    dplyr::filter(.data$equation %in% series)

  # Graph:
  ggplot_add <- list(
    switch(geom,
      "bar" = inject(ggplot2::geom_bar(aes(fill = .data$serie),
        stat = "identity", !!!args_geom
      )),
      "area" = inject(ggplot2::geom_area(aes(fill = .data$serie),
        !!!args_geom
      )),
      "line" = list(
        inject(ggplot2::geom_line(aes(color = .data$serie), !!!args_geom)),
        ggplot2::geom_point(aes(color = .data$serie), ...)
      ),
      stop("Invalid `geom` argument. Choose 'bar', 'area' or 'line'")
    )
  )

  ggplot(data, aes(.data$lead, .data$value)) +
    ggplot_add +
    inject(ggplot2::facet_wrap(vars(.data$equation), !!!args_facet)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = "VAR FEVD", x = "Forecast horizon",
      y = "Variance contribution", fill = "Serie"
    )
}
