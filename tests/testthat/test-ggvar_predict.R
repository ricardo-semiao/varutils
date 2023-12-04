# Seed and variables for the tests:
set.seed(91127)

args <- list(
  var = list(
    x = list(var = "vars::VAR(freeny[1:30,-2])"),
    data_test = list(nodata = "NULL", wdata = "freeny[31:39,-2]"),
    n.ahead = list(`9` = "9"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))
  ),
  pred = list(
    x = list(pred = "stats::predict(vars::VAR(freeny[-2]))"),
    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index'))
  )
)

# Tests:
test_combinations("ggvar_predict", args$var, "x=varest")
test_combinations("ggvar_predict", args$pred, "x=varprd")

test_that("'external' args combinations work", {
  expect_doppelganger("external", ggvar_predict(vars::VAR(freeny[1:30, -2]),
    n.ahead = 19,
    ci = 0.5,
    linetypes = "dotted",
    args_facet = list(scales = "free_y"),
    args_line = list(color = "red")
  ))
})

set.seed(NULL)
