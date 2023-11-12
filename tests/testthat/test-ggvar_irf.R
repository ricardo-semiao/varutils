# Seed and variables for the tests:
set.seed(091127) # https://www.national-lottery.co.uk/results

args <- list(
  var = list(
    x = list(var = "vars::VAR(freeny[-2])"),
    series_impulse = list(null1 = "NULL", single1 = "'y'", multi1 = c('y', 'price.index')),
    series_response = list(null2 = "NULL", single2 = "'y'", multi2 = c('y', 'price.index')),
    facet = list(ggplot = "'ggplot'", ggh4x = "'ggh4x'"),
    n.ahead = list(`9` = "9"),
    ci = list(ci = "FALSE")
  ),
  irf = list(
    x = list(irf = "vars::irf(vars::VAR(freeny[-2]))"),
    series_impulse = list(null1 = "NULL", single1 = "'y'", multi1 = c('y', 'price.index')),
    series_response = list(null2 = "NULL", single2 = "'y'", multi2 = c('y', 'price.index')),
    facet = list(ggplot = "'ggplot'", ggh4x = "'ggh4x'"),
    ci = list(ci = "FALSE")
  )
)

# Tests:
test_combinations("ggvar_irf", args$var, "x=varest")
test_combinations("ggvar_irf", args$irf, "x=varirf")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_irf(vars::VAR(freeny[-2]),
    n.ahead = 19, scales = "free_y", independent = "y",
    ci = 0.5, palette = c("pink", "purple", "violet", "magenta")
  ))
})

set.seed(NULL)

# test_active_file()

# Old tests:
# test_that("'internal' args combinations with varest x respect snapshots", {
#  test_combinations("ggvar_irf", args$var) %>% lapply(eval)
# })
#
# test_that("'internal' args combinations with irf x respect snapshots", {
#  test_combinations("ggvar_irf", args$irf) %>% lapply(eval)
# })
