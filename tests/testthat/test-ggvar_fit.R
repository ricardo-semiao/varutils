# Seed and variables for the tests:
set.seed(91127)

args <- list(
  var1 = list(
    x = list(var1 = "vars::VAR(freeny[-2])"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
    compare = list(true = "TRUE", false = "FALSE")
  ),
  var2 = list(
    x = list(var2 = "vars::VAR(EuStockMarkets)"),
    series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI')),
    compare = list(true = "TRUE", false = "FALSE")
  )
)

# Tests:
test_combinations("ggvar_fit", args$var1, "x=var1")
# test_combinations("ggvar_fit", args$var2, "x=var2")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_fit(vars::VAR(freeny[-2]),
    index = -10:28,
    linetypes = c("dashed", "dotted"),
    args_facet = list(ncol = 1),
    args_line = list(linewidth = 2)
  ))
})

set.seed(NULL)
