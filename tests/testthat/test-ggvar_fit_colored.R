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
test_combinations("ggvar_fit_colored", args$var1, "x=var1")
# test_combinations("ggvar_fit_colored", args$var1, "x=var2")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_fit_colored(vars::VAR(freeny[-2]),
    index = -10:28,
    colors = c("pink", "purple", "violet", "magenta"),
    linetypes = c("dashed", "dotted"),
    args_line = list(linewidth = 2)
  ))
})

set.seed(NULL)

# test_active_file()
