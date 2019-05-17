context("test-user_info")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  user_test <- user_info(user_id = "33816646@N06")

  expect_is(user_test, "data.frame")
  # expect_equal(ncol(user_test), 18)
  expect_equal(nrow(user_test), 1)
})

test_that("invalid API keys fails correctly", {
  # if(file.exists("api_key.txt")) {
  #   file.remove("api_key.txt")
  # }
  # expect_error(user_info(user_id = "33816646@N06"), "Enter API key or save using the save_key function")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(user_info(user_id = "33816646@N06"))
})
