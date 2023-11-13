#' @noRd
setup_ggvar_irf <- function(
    x, n.ahead, series_impulse, series_response,
    facet, ci, palette) {
  test$class_arg(x, c("varirf", "varest"))
  test$series(series_impulse, x, "impulse")
  test$series(series_response, x, "response")
  test$categorical_arg(facet, c("ggplot", "ggh4x"))
  test$interval_arg(ci, 0, 1, FALSE)
  stopifnot(
    "`n.ahead` must be supplied with `x` of class 'varest'" =
      inherits(x, "varirf") || is.numeric(n.ahead)
  )

  series_impulse <- series_impulse %||% get_names(x, "impulse")
  series_response <- series_response %||% get_names(x, "response")
  n <- max(length(series_impulse), length(series_response))

  list(
    series_impulse = series_impulse,
    series_response = series_response,
    palette = get_pallete(palette, n)
  )
}

#' Plot Impulse Response Functions of a VAR
#'
#' Plots the result of a \link[vars]{irf} call.
#'
#' @param x A "varest" object to pass to \link[vars]{irf}, or, directly, a "varirf" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to \link[vars]{irf}. Unused if `x` is "varirf".
#' @param series_impulse A character vector with variables to consider for the impulses. Defaults to all (\code{NULL}).
#' @param series_response A character vector with variables to consider for the responses. Defaults to all (\code{NULL}).
#' @param facet The facet "engine" to be used. "ggplot2" for \link[ggplot2]{facet_grid}, "ggh4x" for \link[ggh4x]{facet_grid2}.
#' @param palette A vector of colors (bins, normal curve). See \code{vignette("palettes")}.
#' @param ci The level of confidence for the \link[vars]{irf}. Set to \code{FALSE} to omit.
#' @param ... Further arguments passed to \link[vars]{irf}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param independent For varying the scales of each cell. See \link[ggh4x]{facet_grid2}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_irf(vars::VAR(freeny[-2]), n.ahead = 10, scales = "free_y")
#'
#' @export
ggvar_irf <- function(
    x, n.ahead = NULL, series_impulse = NULL, series_response = NULL,
    ci = 0.95, ...,
    facet = "ggplot", palette = c("black", "blue", "gray"),
    scales = "fixed", independent = "none") {
  # Setup:
  setup <- setup_ggvar_irf(
    x, n.ahead, series_impulse, series_response,
    facet, ci, palette
  )
  reassign <- c("series_impulse", "series_impulse", "palette")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  irf <- if (inherits(x, "varest")) {
    vars::irf(x, series_impulse, series_response, n.ahead,
      boot = !isFALSE(ci), ci = ci, ...
    )
  } else {
    x
  }

  data <- irf %>%
    magrittr::extract(if (!isFALSE(ci)) 1:3 else 1) %>%
    purrr::imap_dfr(function(x, name) {
      data.frame(
        serie = name,
        purrr::imap_dfr(x, ~ data.frame(effect_on = .y, lead = seq_len(nrow(.x)), .x))
      )
    }) %>%
    tidyr::pivot_longer(-c("serie", "effect_on", "lead"),
      names_to = "effect_of", values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = "serie", values_from = "value")

  # Graph:
  ggplot_add <- list(
    if (!isFALSE(ci)) {
      ggplot2::geom_ribbon(aes(ymin = .data$Lower, ymax = .data$Upper),
        fill = palette[3], color = palette[2], linetype = 2
      )
    },
    define_facet(facet, "effect_of", "effect_on", scales, independent)
  )

  ggplot(data, aes(.data$lead, .data$irf)) +
    ggplot_add +
    ggplot2::geom_line(color = palette[1]) +
    ggplot2::geom_hline(yintercept = 0) +
    create_sec_axis() +
    ggplot2::labs(
      title = "VAR Impulse Response Functions",
      x = "Forecast horizon", y = "Effect"
    )
}
