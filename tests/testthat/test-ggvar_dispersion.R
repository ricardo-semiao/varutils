# Seed and variables for the tests:
set.seed(91127)

args <- list(
  var1 = list(
    x = list(var1 = "vars::VAR(freeny[-2])"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))
  ),
  var2 = list(
    x = list(var2 = "vars::VAR(EuStockMarkets)"),
    series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI'))
  )
)

# Tests:
test_combinations("ggvar_dispersion", args$var1, "x=var1")
# test_combinations("ggvar_dispersion", args$var2, "x=var2")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_dispersion(vars::VAR(freeny[-2]),
    args_facet = list(scales = "free_y", ncol = 1),
    args_point = list(alpha = 0.5),
    args_hline = list(color = "red")
  ))
})

set.seed(NULL)
