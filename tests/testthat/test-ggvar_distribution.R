# Seed and variables for the tests:
set.seed(091127) # https://www.national-lottery.co.uk/results

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
    bins = 45, linewidth = 2,
    palette = c("pink", "purple")
  ))
})

set.seed(NULL)

# test_active_file()

# Old tests:
# test_that("'internal' args combinations with data.frame x respect snapshots", {
#  test_combinations("ggvar_distribution", args$df) %>% lapply(eval)
# })
#
# test_that("'internal' args combinations with mts x respect snapshots", {
#  test_combinations("ggvar_distribution", args$mts) %>% lapply(eval)
# })
#
# test_that("'internal' args combinations with varest x respect snapshots", {
#  test_combinations("ggvar_distribution", args$var) %>% lapply(eval)
# })
