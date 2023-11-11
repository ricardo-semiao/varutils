# Seed and variables for the tests:
set.seed(091127) #https://www.national-lottery.co.uk/results

args <- list(
  df = list(x = list(df = "freeny[-2]"),
            series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
            facet = list(ggplot = "'ggplot'", ggh4x = "'ggh4x'"),
            geom = list(seg = "'segment'", area = "'area'")),
  mts = list(x = list(mts = "EuStockMarkets"),
             series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI')),
             facet = list(ggplot = "'ggplot'", ggh4x = "'ggh4x'"),
             geom = list(seg = "'segment'", area = "'area'")),
  var = list(x = list(var = "vars::VAR(freeny[-2])"),
             series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
             facet = list(ggplot = "'ggplot'", ggh4x = "'ggh4x'"),
             geom = list(seg = "'segment'", area = "'area'"))
)

example <- ggvar_ccf(vars::VAR(freeny[-2]), ci = FALSE, lag.max = 9)

# Tests:
test_combinations("ggvar_ccf", args$df, "x=dataframe")
test_combinations("ggvar_ccf", args$mts, "x=mts")
test_combinations("ggvar_ccf", args$var, "x=varest")

test_that("'external' args combinations work", {
  expect_no_error({
    stopifnot("`lag.max` arg didn't worked" = all(unique(example$data$lag) == 0:9))
    stopifnot("`ci = FALSE` arg didn't worked" = all(sapply(example$layers, \(x) !inherits(class(x$geom), "GeomRibbon"))))
  })
  expect_doppelganger("external", ggvar_ccf(freeny[-2], scales = "free_y", independent = "y", alpha = 0.7, ci = 0.5,
                                            palette = c("pink", "purple", "green", "gray")))
})

set.seed(NULL)

# test_active_file()

# Old tests:
#test_that("'internal' args combinations with data.frame x respect snapshots", {
#  test_combinations("ggvar_ccf", args$df) %>% lapply(eval)
#})
#
#test_that("'internal' args combinations with mts x respect snapshots", {
#  test_combinations("ggvar_ccf", args$mts) %>% lapply(eval)
#})
#
#test_that("'internal' args combinations with varest x respect snapshots", {
#  test_combinations("ggvar_ccf", args$var) %>% lapply(eval)
#})
