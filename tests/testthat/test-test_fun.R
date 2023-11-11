# Tests:

x_varest <- vars::VAR(EuStockMarkets)
x_categorical <- 1; x_categorical2 <- 2
x_interval <- 1; x_interval2 <- "2"
x_dataset <- data.frame(1, "a")

test_that("series test works", {
  expect_error(test_fun()$class_arg(x_varest, "character"))
  expect_no_error(test_fun()$class_arg(x_varest, "varest"))
})

test_that("series test works", {
  expect_error(test_fun()$series(c(1,3), x_varest)) #non character series
  expect_error(test_fun()$series(c("undefined column"), x_varest)) #undefined column
})

test_that("index test works", {
  expect_error(test_fun()$index(1:10, n = 11)) #short index
  expect_error(test_fun()$index(factor(1:10), n = 10)) #factor index
  expect_error(test_fun()$index(sample(letters, 100, TRUE), n = 100)) #index with duplicates
})

test_that("categorical_arg test works", {
  expect_error(test_fun()$categorical_arg(x_categorical2, 1)) #one option
  expect_no_error(test_fun()$categorical_arg(x_categorical, 1)) #one option
  #expect_error(test_fun()$categorical_arg("2", 1:2))
})

test_that("boolean_arg test works", {
  expect_error(test_fun()$boolean_arg(NA)) #NA (logical) arg
})

test_that("interval_arg test works", {
  expect_error(test_fun()$interval_arg(x_interval, 2, 3, TRUE)) #TRUE should be =/= 1
  expect_no_error(test_fun()$interval_arg(x_interval, 2, 3, 1)) #one option
  expect_no_error(test_fun()$interval_arg(x_interval, 0, 3, NULL)) #multiple options
  expect_error(test_fun()$interval_arg(x_interval2, 1, 3, NULL)) #multiple options
})

test_that("dataset_arg test works", {
  expect_warning(test_fun()$dataset_arg(x_dataset)) #non numeric
})

# test_active_file()
