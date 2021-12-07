library(testthat)
test_that("f03a", {
  expect_equal(f03a(example_data_03(1)), 198)
})
test_that("f03b", {
  expect_equal(f03b(example_data_03(1)), 230)
})
