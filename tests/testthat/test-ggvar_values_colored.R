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
test_combinations("ggvar_values_colored", args$df, "x=dataframe")
test_combinations("ggvar_values_colored", args$mts, "x=mts")
test_combinations("ggvar_values_colored", args$var, "x=varest")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_values_colored(vars::VAR(freeny[-2]), index = -10:27,
                                                       palette = c("pink", "purple", "violet", "magenta")))
})

set.seed(NULL)

# test_active_file()

# Old tests:
