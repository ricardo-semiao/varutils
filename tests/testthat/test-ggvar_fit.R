# Seed and variables for the tests:
set.seed(091127) # https://www.national-lottery.co.uk/results

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
    linetypes = c("dashed", "dotted"), scales = "free_y",
    ncol = 2, palette = c("pink")
  ))
})

set.seed(NULL)

# test_active_file()

# Old tests:
# test_that("'internal' args combinations with varest x respect snapshots", {
#  test_combinations("ggvar_fit", args$var1) %>% lapply(eval)
#  #test_combinations("ggvar_distribution", args$var2) %>% lapply(eval)
# })
