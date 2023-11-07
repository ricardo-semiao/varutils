#' Plot for Forecast Error Variance Decomposition of a VAR
#'
#' Plots the result of a \link[vars]{fevd} call.
#'
#' @param x A "varest" objecto to pass to \link[vars]{fevd}, or, directly, a "varfevd" object.
#' @param n.ahead An interger. The size of the forecast horizon, passed to \link[vars]{fevd}. Unused if `x` is "varfevd".
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param geom The ggplot geom used to create the plot, "bar" for \link[ggplot2]{geom_bar} (the default), "area" for \link[ggplot2]{geom_area}, or "line" for \link[ggplot2]{geom_line}.
#' @param palette A vector of colors (line, conf. interval). See \code{vignette("palettes")}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Aditional arguments passed to the ggplot geom defined by \code{geom}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_fevd(vars::VAR(EuStockMarkets), n.ahead = 10)
#'
#' @export
ggvar_fevd <- function(
    x, n.ahead = NULL, series = NULL, geom = "bar",
    palette = NULL, scales = "fixed", ncol = 1, ...
  ) {
  # Initial tests:
  stopifnot(inherits(x, c("varfevd", "varest")))

  # Create values:
  series <- series %||% names(x$varresult)
  palette <- get_pallete(palette, length(series))

  if (is.null(n.ahead)) {
    if (inherits(x, "varfevd")) n.ahead <- nrow(x[[1]]) else stop("`n.ahead` must be supplied with `x` of class 'varest'")
  }

  ggplot_add <- list(
    switch(
      geom,
      "bar" = list(ggplot2::geom_bar(aes(fill = .data$serie), stat = "identity", ...)),
      "area" = list(ggplot2::geom_area(aes(fill = .data$serie), ...)),
      "line" = list(ggplot2::geom_line(aes(color = .data$serie), ...),
                    ggplot2::geom_point(aes(color = .data$serie), shape = 1)),
      stop("Invalid `geom` argument. Choose 'bar', 'area' or 'line'")
    )
  )

  # Data - fevd:
  fevd <- if (inherits(x, "varest")) vars::fevd(x, n.ahead) else x

  data <- fevd %>%
    purrr::imap_dfr(~ as.data.frame(.x) %>% dplyr::mutate(equation = .y, lead = 1:nrow(.))) %>%
    tidyr::pivot_longer(-c(.data$equation, .data$lead), names_to = "serie", values_to = "value") %>%
    dplyr::filter(equation %in% series & serie %in% series)

  # Graph:
  ggplot(data, aes(.data$lead, .data$value)) +
    ggplot_add +
    ggplot2::facet_wrap(vars(.data$equation), scales = scales, ncol = ncol) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(title = "VAR Forecast Error Variance Decomposition", x = "Forecast horizon", y = "Variance contribution")
}
