library(testthat)
test_that("f02a works", {
  expect_equal(f02a(example_data_02(example = 1)), 150)
})

test_that("f02b works", {
  expect_equal(f02b(example_data_02(example = 2)), 900)
})
