# Seed and variables for the tests:
set.seed(91127)

args <- list(
  df = list(
    x = list(df = "freeny[-2]"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
    plot_normal = list(true = "TRUE", false = "FALSE")
  ),
  mts = list(
    x = list(mts = "EuStockMarkets"),
    series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI')),
    plot_normal = list(true = "TRUE", false = "FALSE")
  ),
  var = list(
    x = list(var = "vars::VAR(freeny[-2])"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
    plot_normal = list(true = "TRUE", false = "FALSE")
  )
)

# Tests:
test_combinations("ggvar_distribution", args$df, "x=dataframe")
test_combinations("ggvar_distribution", args$mts, "x=mts")
test_combinations("ggvar_distribution", args$var, "x=varest")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_distribution(vars::VAR(freeny[-2]),
    args_histogram = list(bins = 45),
    args_line = list(linewidth = 2),
    args_facet = list(scales = "free_y", ncol = 1)
  ))
})

set.seed(NULL)
