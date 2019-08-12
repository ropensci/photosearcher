context("test-user_info")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  interesting_test <- interesting_list(date = "2019-01-01")

  expect_is(interesting_test, "data.frame")
  expect_equal(ncol(interesting_test), 57)
})

test_that("invalid date provides error", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")

  expect_error(interesting_list(date = "not a real date"),
               "Not a valid date string")

})

test_that("invalid API keys fails correctly", {
  # if(file.exists("api_key.txt")) { file.remove("api_key.txt") }
  # expect_error(user_info(user_id = "33816646@N06"), "Enter API key or save
  # using the save_key function")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(interesting_list(date = "2019-01-01"))
})
