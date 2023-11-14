# Seed and variables for the tests:
set.seed(91127)

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
    n.ahead = 19,
    args_facet = list(scales = "free_y", ncol = 2),
    args_geom = list(alpha = 0.5),
    colors = c("pink", "purple", "violet", "magenta")
  ))
})

set.seed(NULL)
