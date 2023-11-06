suported_palettes <- list(
    base = c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"),
    viridis = c("viridis", "magma", "inferno", "plasma", "cividis", "mako", "rocket", "turbo"),
    RColorBrewer = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
                     "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Accent", "Dark2",
                     "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "BrBG", "PiYG", "PRGn", "PuOr",
                     "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
    ggsci = c("pal_npg", "pal_aaas", "pal_nejm", "pal_lancet", "pal_jama", "pal_jco", "pal_ucscgb", "pal_d3",
              "pal_locuszoom", "pal_igv", "pal_cosmic", "pal_uchicago", "pal_startrek", "pal_tron", "pal_futurama",
              "pal_rickandmorty", "pal_simpsons", "pal_flatui", "pal_frontiers", "pal_gsea", "pal_material")
  )

#knitr::kable(sapply(suported_palettes, \(x) paste(x, collapse = ", ")))
