#' Plot for Forecast Error Variance Decomposition of a VAR
#'
#' Plots the result of a \link[vars]{fevd} call.
#'
#' @param x A "varest" object to pass to \link[vars]{fevd}, or, directly, a "varfevd" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to \link[vars]{fevd}. Unused if `x` is "varfevd".
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param geom The ggplot geom used to create the plot, "bar" for \link[ggplot2]{geom_bar} (the default), "area" for \link[ggplot2]{geom_area}, or "line" for \link[ggplot2]{geom_line}.
#' @param palette A vector of colors (line, conf. interval). See \code{vignette("palettes")}.
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
    x, n.ahead = NULL, series = NULL, geom = "bar",
    palette = NULL, scales = "fixed", ncol = 1, ...) {
  # Initial tests:
  test$class_arg(x, c("varfevd", "varest"))
  test$series(series, x)
  test$categorical_arg(geom, c("bar", "area", "line"))

  # Create values:
  series <- series %||% if (inherits(x, "varest")) names(x$varresult) else names(x)
  palette <- get_pallete(palette, if (inherits(x, "varest")) x$K else length(x))

  if (inherits(x, "varest")) {
    if (is.null(n.ahead) || n.ahead == 0) stop("`n.ahead` must be supplied with `x` of class 'varest'")
  } else if (inherits(x, "varfevd")) {
    if (is.numeric(n.ahead)) warning("`n.ahead` suplied with 'varfevd' x was set to `nrow(x[[1]])`")
    n.ahead <- nrow(x[[1]])
  }

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

  # Data - fevd:
  fevd <- if (inherits(x, "varest")) vars::fevd(x, n.ahead) else x

  data <- fevd %>%
    purrr::imap_dfr(~ as.data.frame(.x) %>% dplyr::mutate(equation = .y, lead = 1:nrow(.))) %>%
    tidyr::pivot_longer(-c("equation", "lead"), names_to = "serie", values_to = "value") %>%
    dplyr::filter(equation %in% series)

  # Graph:
  ggplot(data, aes(.data$lead, .data$value)) +
    ggplot_add +
    ggplot2::facet_wrap(vars(.data$equation), scales = scales, ncol = ncol) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(title = "VAR FEVD", x = "Forecast horizon", y = "Variance contribution", fill = "Serie")
}
