# Seed and variables for the tests:
set.seed(091127) #https://www.national-lottery.co.uk/results

args <- list(
  var = list(x = list(var = "vars::VAR(freeny[1:30,-2])"),
             data_test = list(nodata = "NULL", wdata = "freeny[31:39,-2]"),
             n.ahead = list(`9` = "9"),
             series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))),
  pred = list(x = list(pred = "stats::predict(vars::VAR(freeny[-2]))"),
              series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')))
)

# Tests:
test_combinations("ggvar_predict", args$var, "x=varest")
test_combinations("ggvar_predict", args$pred, "x=varprd")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_predict(vars::VAR(freeny[1:30,-2]), n.ahead = 19, scales = "free_y", ncol = 2,
                                                ci = 0.5, alpha = 0.5, linetypes = "dotted",
                                                palette = c("pink", "purple", "violet", "magenta")))
})

set.seed(NULL)

# test_active_file()

# Old tests:
#test_that("'internal' args combinations with varest x respect snapshots", {
#  test_combinations("ggvar_fevd", args$var) %>% lapply(eval)
#})
#
#test_that("'internal' args combinations with varfevd x respect snapshots", {
#  test_combinations("ggvar_fevd", args$fevd) %>% lapply(eval)
#})
