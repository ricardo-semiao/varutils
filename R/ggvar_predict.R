#' Plot the Predicted Values of a VAR
#'
#' Plots the result of a \link[vars]{predict.varest} call. Has an option to overlay it with the true variables, if provided a test dataset.
#'
#' @param x A "varest" object to get predictions from, or, directly, a "varprd" object.
#' @param data_test A test dataset (object coercible to data.frame), with the actual series values.
#' @param n.ahead An interger. The number of periods to predict, passed to \link[stats]{predict}.
#' @param series A character vector with variables to consider. Defaults to all (\code{NULL}).
#' @param index A vector of labels to the x-axis, normally dates. Must have length equal to \code{n.ahead}. Defaults to a numeric sequence.
#' @param palette A vector of colors. Just one for \code{ggvar_fit}, one for each variable for \code{ggvar_fit_colored}. See \code{vignette("palettes")}.
#' @param linetype A linetypes for \link[ggplot2]{geom_ribbon}.
#' @param alpha A double. The alpha aesthetic for the fill of \link[ggplot2]{geom_ribbon}.
#' @param scales "fixed" (the default), "free", "free_x" or "free_y". passed to \link[ggplot2]{facet_wrap}.
#' @param ncol An interger. The number of facet columns, passed to \link[ggplot2]{facet_wrap}.
#' @param ... Aditional arguments passed to \link[ggplot2]{geom_line}.
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_predict(vars::VAR(EuStockMarkets), n.ahead = 10)
#' ggvar_predict(vars::VAR(EuStockMarkets[1:1800,]), EuStockMarkets[1801:1860,])
#'
#' @export
ggvar_predict <- function(
    x, data_test = NULL, n.ahead = nrow(data_test), series = NULL, index = 1:n.ahead,
    palette = c("blue", "black", "darkblue", "gray"), linetype = "dashed", alpha = 0.8, scales = "fixed", ncol = 1, ...
  ) {
  # Initial tests:
  stopifnot(inherits(x, c("varest", "varprd")))
  if (is.null(data_test)) {
    n.ahead %||% stop("`n.ahead` must be provided with NULL `data_test`")
  } else {
    stopifnot(inherits(data_test, c("data.frame", "matrix")), n.ahead == nrow(data_test))
  }

  # Create values:
  palette <- get_pallete(palette, 4)
  series <- series %||% names(x$varresult)

  # Data - predictions:
  pred <- if (inherits(x, "varest")) stats::predict(x, n.ahead = n.ahead) else x

  data_pred <- pred$fcst %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    dplyr::mutate(serie = rep(series, each = n.ahead), index = rep(index, pred$model$K)) %>%
    dplyr::rename(prediction = fcst)

  data_pred <- if (is.null(data_test)) {
    data_pred %>%
      tidyr::pivot_longer(c(.data$prediction), values_to = "value", names_to = "type")
  } else {
    data_test %>%
      as.data.frame() %>%
      dplyr::mutate(index = index) %>%
      tidyr::pivot_longer(-.data$index, values_to = "actual", names_to = "serie") %>%
      dplyr::full_join(data_pred, by = c("index", "serie")) %>%
      tidyr::pivot_longer(c(.data$prediction, .data$actual), values_to = "value", names_to = "type")
  }

  # Graph:
  ggplot(data_pred, aes(.data$index, .data$value)) +
    ggplot2::geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper),
                         fill = palette[4], color = palette[3], linetype = linetype, alpha = alpha) +
    ggplot2::geom_line(aes(color = .data$type), ...) +
    ggplot2::facet_wrap(ggplot2::vars(.data$serie), scales = scales, ncol = ncol) +
    ggplot2::scale_color_manual(values = palette[1:2], guide = if (is.null(data_test)) "none" else "legend") +
    ggplot2::labs(title = "VAR Predicted Values", x = "Index", y = "Values", color = "Series")
}
