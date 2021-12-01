library(testthat)
test_that("1a works", {
  expect_equal(f01a(example_data_01(example = 1)), 7)
})
test_that("1b works", {
  expect_equal(f01b(example_data_01(example = 2)), 5)
})
