# Seed and variables for the tests:
set.seed(091127) # https://www.national-lottery.co.uk/results

args <- list(
  var = list(
    x = list(var = "vars::VAR(freeny[-2])"),
    series_impulse = list(null1 = "NULL", single1 = "'y'", multi1 = c('y', 'price.index')),
    series_response = list(null2 = "NULL", single2 = "'y'", multi2 = c('y', 'price.index')),
    n.ahead = list(`9` = "9"),
    ci = list(ci = "FALSE")
  ),
  irf = list(
    x = list(irf = "vars::irf(vars::VAR(freeny[-2]))"),
    series_impulse = list(null1 = "NULL", single1 = "'y'", multi1 = c('y', 'price.index')),
    series_response = list(null2 = "NULL", single2 = "'y'", multi2 = c('y', 'price.index')),
    ci = list(ci = "FALSE")
  )
)

# Tests:
test_combinations("ggvar_irf", args$var, "x=varest")
test_combinations("ggvar_irf", args$irf, "x=varirf")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_irf(vars::VAR(freeny[-2]),
    n.ahead = 19,
    ci = 0.5,
    facet = "ggh4x",
    args_line = list(color = "red"),
    args_hline = list(linewidth = 2),
    args_facet = list(scales = "free_y", independent = "y")
  ))
})

set.seed(NULL)
