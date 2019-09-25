context("test-user_info")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  user_test <- user_info(user_id = "33816646@N06")

  expect_is(user_test, "data.frame")
  expect_equal(nrow(user_test), 1)
})

#test errors
test_that("invalid ID provides error", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  expect_warning(user_info(user_id = c("1", "33816646@N06")),
                 "User ID 1 is not valid")

})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(user_info(user_id = "33816646@N06"))
})
