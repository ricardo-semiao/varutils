#' Plot the Predicted Values of a VAR
#'
#' Plots the result of a \link[vars]{predict.varest} call. Has an option to overlay it with the true variables, if provided a test dataset.
#'
#' @param x A "varest" object to get predictions from, or, directly, a "varprd" object.
#' @param data_test A test data set (object coercible to data.frame), with the actual series values. If \code{NULL}, no comparison is made.
#' @param n.ahead An integer. The number of periods to predict, passed to \link[stats]{predict}. Defaults to \code{nrow(data_test)} or the horizon of the "varprd" object (\code{NULL}).
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param index A vector of labels to the x-axis, normally dates. Must have length equal to \code{n.ahead}. Defaults to a numeric sequence.
#' @param ci The level of confidence for the ACF confidence interval. Set to \code{FALSE} to omit. Passed to \link[stats]{predict}.
#' @param dumvar Exogenous variables to be passed to \link[stats]{predict}.
#' @param palette A vector of colors. Just one for \code{ggvar_fit}, one for each variable for \code{ggvar_fit_colored}. See \code{vignette("palettes")}.
#' @param linetypes A line types for \link[ggplot2]{geom_ribbon}.
#' @param alpha A double. The alpha aesthetic for the fill of \link[ggplot2]{geom_ribbon}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An integer. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Additional arguments passed to \link[ggplot2]{geom_line}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_predict(stats::predict(vars::VAR(freeny[-2])))
#' ggvar_predict(vars::VAR(freeny[-2][1:30,]), freeny[-2][31:39,])
#'
#' @export
ggvar_predict <- function(
    x, data_test = NULL, n.ahead = NULL, series = NULL, index = 1:n.ahead,
    ci = 0.95, dumvar = NULL,
    palette = c("blue", "black", "darkblue", "gray"), linetypes = "dashed", alpha = 0.8, scales = "fixed", ncol = 1, ...
  ) {
  # Initial tests:
  test$class_arg(x, c("varest", "varprd"))
  test$class_arg(data_test, c("data.frame", "matrix", "NULL"))
  data_test <- test$dataset_arg(data_test)
  test$series(series, x)
  test$interval_arg(ci, 0, 1, FALSE)

  if (inherits(x, "varprd")) {
    if (!is.null(n.ahead)) warning("`x` of class 'varprd', ignoring `n.ahead`")
    n.ahead <- nrow(x$fcst[[1]])
  } else {
    n.ahead <- n.ahead %||% nrow(data_test) %||% stop("`n.ahead` must be supplied with `x` of class 'varest' and `data_test = NULL`")
  }

  if (!is.null(data_test) && nrow(data_test) != n.ahead) stop("`data_test` with incorrect number of rows")

  test$index(index, n = n.ahead)

  # Create values:
  palette <- get_pallete(palette, 4)
  series <- series %||% if (inherits(x, "varest")) names(x$varresult) else names(x$fcst)

  ggplot_add <- list(
    if (!isFALSE(ci))  ggplot2::geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper),
                                            fill = palette[4], color = palette[3], linetype = linetypes[1], alpha = alpha)
  )

  # Data - predictions:
  pred <- if (inherits(x, "varest")) stats::predict(x, n.ahead = n.ahead, ci = ci, dumvar = dumvar) else x

  data_pred <- pred$fcst[series] %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    dplyr::mutate(serie = rep(series, each = n.ahead), index = rep(index, length(series))) %>%
    dplyr::rename(prediction = fcst)

  data_pred <- if (is.null(data_test)) {
    data_pred %>%
      tidyr::pivot_longer(c("prediction"), values_to = "value", names_to = "type")
  } else {
    data_test %>%
      as.data.frame() %>%
      dplyr::mutate(index = index) %>%
      tidyr::pivot_longer(-c("index"), values_to = "actual", names_to = "serie") %>%
      dplyr::full_join(data_pred, by = c("index", "serie")) %>%
      tidyr::pivot_longer(c("prediction", "actual"), values_to = "value", names_to = "type")
  }

  # Graph:
  ggplot(data_pred, aes(.data$index, .data$value)) +
    ggplot_add +
    ggplot2::geom_line(aes(color = .data$type), ...) +
    ggplot2::facet_wrap(vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::scale_color_manual(values = palette[1:2], guide = if (is.null(data_test)) "none" else "legend") +
    ggplot2::labs(title = "VAR Predicted Values", x = "Index", y = "Values", color = "Series")
}
