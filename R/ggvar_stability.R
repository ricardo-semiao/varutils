#' @noRd
setup_ggvar_stability <- function(x, series) {
  test$class_arg(x, c("varest", "varstabil"))
  test$series(series, x)

  list(
    series = series %||% get_names(x)
  )
}

#' Plot for Structural Stability of a VAR
#'
#' Plots the result of a \link[vars]{stability} call. Confidence intevals are
#'  calculated using \link[strucchange]{boundary}.
#'
#' @param x A "varest" object to pass to \link[vars]{stability}, or, directly, a
#'  "varstabil" object.
#' @eval param_series()
#' @param ci The level of confidence for the \link[strucchange]{boundary}.
#' @param ... Further arguments passed to \link[strucchange]{boundary}.
#' @eval param_args(c("geom_line", "geom_hline", "facet_wrap"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_stability(vars::VAR(freeny[-2]))
#'
#' @export
ggvar_stability <- function(
    x, series = NULL,
    ci = 0.95, ...,
    args_line = list(),
    args_hline = list(linetype = 2, color = "blue"),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_stability(x, series)
  reassign <- c("series", "series", "series")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  stab <- if (inherits(x, "varest")) {
    vars::stability(x)$stability
  } else {
    x$stability
  }

  interval <- strucchange::boundary(stab[[1]],
    alpha = 1 - ci, alt.boundary = FALSE, functional = "max"
  )

  data <- purrr::imap_dfr(stab, function(x, name) {
    data.frame(
      equation = name,
      index = stats::time(x$process),
      value = as.numeric(x$process)
    )
  })

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    inject(ggplot2::geom_line(!!!args_line)) +
    inject(ggplot2::geom_hline(yintercept = c(-interval, interval), !!!args_hline)) +
    inject(ggplot2::facet_wrap(vars(.data$equation), !!!args_facet)) +
    ggplot2::labs(
      title = "VAR Structural Stability Analisys", x = "Index", y = "Values"
    )
}
