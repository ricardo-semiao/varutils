# Seed and variables for the tests:
set.seed(091127) # https://www.national-lottery.co.uk/results

args <- list(
  var = list(
    x = list(var = "vars::VAR(freeny[-2])"),
    n.ahead = list(`9` = "9"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
    geom = list(bar = "'bar'", area = "'area'", line = "'line'")
  ),
  fevd = list(
    x = list(var = "vars::fevd(vars::VAR(freeny[-2]))"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
    geom = list(bar = "'bar'", area = "'area'", line = "'line'")
  )
)

# Tests:
test_combinations("ggvar_fevd", args$var, "x=varest")
test_combinations("ggvar_fevd", args$fevd, "x=varfevd")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_fevd(vars::VAR(freeny[-2]),
    n.ahead = 19, scales = "free_y", ncol = 2,
    palette = c("pink", "purple", "violet", "magenta")
  ))
})

set.seed(NULL)

# test_active_file()

# Old tests:
# test_that("'internal' args combinations with varest x respect snapshots", {
#  test_combinations("ggvar_fevd", args$var) %>% lapply(eval)
# })
#
# test_that("'internal' args combinations with varfevd x respect snapshots", {
#  test_combinations("ggvar_fevd", args$fevd) %>% lapply(eval)
# })
