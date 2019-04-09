context("test-user_info")

test_that("fails correctly", {
  expect_error(user_info(), "provide user id")
})
