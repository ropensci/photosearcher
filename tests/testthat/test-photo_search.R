context("test-photo_search")

test_that("fails correctly", {
  expect_error(photo_search(min_taken = NULL))
  expect_error(photo_search(max_taken = NULL))
})
