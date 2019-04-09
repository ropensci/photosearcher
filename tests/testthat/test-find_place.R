context("test-find_place")

test_that("fails correctly", {
  expect_error(find_place(), "provide a place")
})
