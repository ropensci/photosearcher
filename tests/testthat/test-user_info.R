context("test-user_info")

test_that("fails correctly", {
  expect_error(user_info(), "provide user id")
})

test_that("output is correct", {
  skip_on_cran()

  user_test <- user_info(user_id = "33816646@N06", api_key = test_key)

  expect_is(user_test, "data.frame")
  # expect_equal(ncol(user_test), 18)
  expect_equal(nrow(user_test), 1)
})

test_that("invalid API keys fails correctly", {
  expect_error(user_info(api_key = NULL, user_id = "33816646@N06"), "Enter API key or save using the save_key function")

  skip_on_cran()
  expect_error(user_info(api_key = "notarealkey", user_id = "33816646@N06"))
})
