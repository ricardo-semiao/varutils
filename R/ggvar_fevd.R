#' @noRd
setup_ggvar_fevd <- function(x, n.ahead, series, geom, palette) {
  test$class_arg(x, c("varfevd", "varest"))
  test$series(series, x)
  test$categorical_arg(geom, c("bar", "area", "line"))

  list(
    series = series %||% get_names(x),
    palette = get_pallete(palette, length(get_names(x))),
    n.ahead = n.ahead
  )
}

#' Plot for Forecast Error Variance Decomposition of a VAR
#'
#' Plots the result of a \link[vars]{fevd} call.
#'
#' @param x A "varest" object to pass to \link[vars]{fevd}, or, directly, a "varfevd" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to \link[vars]{fevd}. Unused if `x` is "varfevd".
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param geom The ggplot geom used to create the plot, "bar" for \link[ggplot2]{geom_bar} (the default), "area" for \link[ggplot2]{geom_area}, or "line" for \link[ggplot2]{geom_line}.
#' @param palette A vector of colors for each variable. See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An integer. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Additional arguments passed to the ggplot geom defined by \code{geom}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_fevd(vars::VAR(freeny[-2]), n.ahead = 10)
#'
#' @export
ggvar_fevd <- function(
    x, n.ahead = NULL, series = NULL,
    geom = "bar", palette = NULL,
    scales = "fixed", ncol = 1, ...) {
  # Setup:
  setup <- setup_ggvar_fevd(x, n.ahead, series, geom, palette)
  reassign <- c("n.ahead", "series", "palette")
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
      "bar" = list(ggplot2::geom_bar(aes(fill = .data$serie), stat = "identity", ...)),
      "area" = list(ggplot2::geom_area(aes(fill = .data$serie), ...)),
      "line" = list(
        ggplot2::geom_line(aes(color = .data$serie), ...),
        ggplot2::geom_point(aes(color = .data$serie), shape = 1)
      ),
      stop("Invalid `geom` argument. Choose 'bar', 'area' or 'line'")
    )
  )

  ggplot(data, aes(.data$lead, .data$value)) +
    ggplot_add +
    ggplot2::facet_wrap(vars(.data$equation), scales = scales, ncol = ncol) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      title = "VAR FEVD", x = "Forecast horizon",
      y = "Variance contribution", fill = "Serie"
    )
}
