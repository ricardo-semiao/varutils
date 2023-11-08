x <- vars::VAR(EuStockMarkets)

test_that("initial tests work", {
  expect_error(ggvar_fit(lm(cyl ~ mpg, mtcars)))
  expect_error(ggvar_fit(x, series = c(1,3)))
  expect_error(ggvar_fit(x, index = 1:10))
  expect_error(ggvar_fit(x, index = factor(1:x$obs)))
  expect_error(ggvar_fit(x, index = sample(letters, x$obs, TRUE)))
  expect_error(ggvar_fit(x, compare = NA))
})

test_that("index argument work", {
  expect_no_error(ggvar_fit(x, index = -10:(x$totobs - 11)))
  expect_no_error(ggvar_fit(x, index = as.character(1:x$totobs)))
})
