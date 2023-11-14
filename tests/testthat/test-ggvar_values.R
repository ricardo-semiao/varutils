# Seed and variables for the tests:
set.seed(91127)

args <- list(
  df = list(
    x = list(df = "freeny[-2]"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))
  ),
  mts = list(
    x = list(mts = "EuStockMarkets"),
    series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI'))
  ),
  var = list(
    x = list(var = "vars::VAR(freeny[-2])"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))
  )
)

# Tests:
test_combinations("ggvar_values", args$df, "x=dataframe")
test_combinations("ggvar_values", args$mts, "x=mts")
test_combinations("ggvar_values", args$var, "x=varest")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_values(vars::VAR(freeny[-2]),
    index = -10:27,
    args_line = list(linewidth = 2),
    args_facet = list(scales = "free_y", ncol = 2)
  ))
})

set.seed(NULL)
