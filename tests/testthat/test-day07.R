library(testthat)
test_that("f07a", {
  expect_equal(f07a(example_data_07()), 37)
})
test_that("f07b", {
  expect_equal(f07b(example_data_07()), 168)
})
