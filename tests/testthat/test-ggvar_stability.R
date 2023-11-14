# Seed and variables for the tests:
set.seed(091127) # https://www.national-lottery.co.uk/results

args <- list(
  var = list(
    x = list(var = "vars::VAR(freeny[-2])"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))
  ),
  stab = list(
    x = list(stab = "vars::stability(vars::VAR(EuStockMarkets))"),
    series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI'))
  )
)

# Tests:
test_combinations("ggvar_stability", args$var, "x=varest")
test_combinations("ggvar_stability", args$stab, "x=varstabil")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_stability(vars::VAR(freeny[1:30, -2]),
    ci = 0.5,
    args_facet = list(scales = "free_y", ncol = 1),
    args_line = list(color = "red")
  ))
})

set.seed(NULL)

# test_active_file()
