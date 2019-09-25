context("test-user_info")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  interesting_test <- interesting_list(date = "2019-01-01")

  expect_is(interesting_test, "data.frame")
  expect_equal(ncol(interesting_test), 57)
})

test_that("invalid date provides error", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  expect_error(interesting_list(date = "1900"),
               "Not a valid date string")

})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(interesting_list(date = "2019-01-01"))
})
