get_pallete <- function(palette, n, ...) {
  {
    if (is.null(palette)) { #ggplot's default palette
      grDevices::hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
    }

    else if (length(palette) == 1 && grepl("::", palette)) { #pgk::palette
      pkg_pal <- strsplit(palette, "::")[[1]]

      if (!rlang::is_installed(pkg_pal[1])) {
        stop(paste0("Package ", pkg_pal[1], " isn't installed"))
      }
      if (!pkg_pal[2] %in% suported_palettes[[pkg_pal[1]]]) {
        stop(paste0("With package ", pkg_pal[1], ", palette should be one of:\n    ", suported_palettes[[pkg_pal[1]]]))
      }

      if (pkg_pal[1] == "ggplot2") { pkg_pal[2]
      } else if (pkg_pal[1] == "RColorBrewer") { RColorBrewer::brewer.pal(n, pkg_pal[2], ...)
      } else if (pkg_pal[1] %in% c("base", "viridis", "ggsci")) { match.fun(palette)(n, ...)[1:n] }
    }


    else if (is.atomic(palette)) { palette } #vector of colors

    else {stop("Unrecognized `palette` argument")}
  }
}
