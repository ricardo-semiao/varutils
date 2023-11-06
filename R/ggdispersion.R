ggdispersion <- function(x, ncol = 1, alpha = 0.5, palette = c(points = "black", xaxis = "black")){
  stopifnot(inherits(x, "varest"))

  data <- data.frame(Residuals.. = residuals(x), Fitted.. = fitted(x))

  data %>%
    tidyr::pivot_longer(dplyr::everything(), names_sep = "\\.\\.\\.", names_to = c(".value", "Variable")) %>%
    ggplot(aes(.data$Fitted, .data$Residuals)) +
      ggplot2::geom_point(color = palette[1], alpha = alpha) +
      ggplot2::geom_hline(yintercept = 0, color = palette[2]) +
      ggplot2::facet_wrap(ggplot2::vars(.data$Variable), scales = "free", ncol = ncol) +
      ggplot2::labs(title = "VAR Residuals Dispersion")
}
