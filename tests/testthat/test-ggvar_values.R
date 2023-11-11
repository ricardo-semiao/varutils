# Seed and variables for the tests:
set.seed(091127) #https://www.national-lottery.co.uk/results

args <- list(
  df = list(x = list(df = "freeny[-2]"),
            series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))),
  mts = list(x = list(mts = "EuStockMarkets"),
             series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI'))),
  var = list(x = list(var = "vars::VAR(freeny[-2])"),
             series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')))
)

# Tests:
test_combinations("ggvar_values", args$df, "x=dataframe")
test_combinations("ggvar_values", args$mts, "x=mts")
test_combinations("ggvar_values", args$var, "x=varest")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_values(vars::VAR(freeny[-2]), index = -10:27,
                                               scales = "free_y", ncol = 2, palette = c("pink")))
})

set.seed(NULL)

# test_active_file()

# Old tests:
