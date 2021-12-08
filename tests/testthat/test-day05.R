library(testthat)
test_that("f05a", {
  expect_equal(f05a(example_data_05()), 5)
})
test_that("f05b", {
  expect_equal(f05b(example_data_05()), 12)
})
