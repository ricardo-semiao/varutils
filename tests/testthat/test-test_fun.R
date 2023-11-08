# In progress

#x_varest <- vars::VAR(EuStockMarkets)
#
#test_that("series test works", {
#  expect_error(test_fun()$series(c(1,3), x_varest)) #non character series
#  expect_error(test_fun()$series(c("undefined column"), x_varest)) #undefined column
#})
#
#test_that("index test works", {
#  expect_error(test_fun()$index(1:10, x_varest)) #short index
#  expect_error(test_fun()$index(factor(1:x_varest$totobs), x_varest)) #factor index
#  expect_error(test_fun()$index(sample(letters, x_varest$totobs, TRUE), x_varest)) #index with duplicates
#})
#
#test_that("compare test works", {
#  expect_error(test_fun()$compare(NA)) #NA (logical) compare
#})
