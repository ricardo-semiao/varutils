#' Plot Information Criteria for VAR Lag Selection
#'
#' Plots the result of a call to \link[vars]{VARselect}.
#'
#' @param x The result of a \link[vars]{VARselect} call.
#' @param criteria The criteria to be considered. Any of "AIC", "HQ", "SC",
#'  and "FPE".
#' @param trans A transformation to apply to each criteria result (vector). Can
#'  be a function, "none" (the default), or "index" to create index numbers.
#' @eval param_args("geom_line")
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_select(vars::VARselect(freeny[-2]))
#'
#' @export
ggvar_select <- function(
    x, criteria = c("AIC", "HQ", "SC", "FPE"), trans = "none",
    args_line = list()) {
  # Setup:
  criteria <- paste0(criteria, "(n)")
  # ...

  # Data:
  data <- t(x$criteria) %>%
    apply(., 2, \(x) if (trans == "index") {x/x[1]} else {x}) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::any_of(criteria)) %>%
    dplyr::mutate(lag = 1:nrow(.)) %>%
    tidyr::pivot_longer(-"lag")

  # Graph:
  ggplot(data, aes(.data$lag, .data$value, color = .data$name)) +
    inject(ggplot2::geom_line(!!!args_line)) +
    ggplot2::labs(
      title = "Information Criteria for Each Lag", color = "Criteria",
      x = "Lag", y = "value"
    )
}
