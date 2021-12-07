library(testthat)
test_that("f04a", {
  expect_equal(f04a(example_data_04()), 4512)
})
test_that("f04b", {
  expect_equal(f04b(example_data_04()), 1924)
})
