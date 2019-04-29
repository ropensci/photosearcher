context("test-user_info")

test_that("fails correctly", {
  expect_error(user_info(), "provide user id")
})

test_that("output is correct", {
  user_test <- user_info(user_id = "33816646@N06", api_key = "3863b3b3d95341cff763f05e989012b7")

  expect_is(user_test, "data.frame")
  #expect_equal(ncol(user_test), 18)
  expect_equal(nrow(user_test), 1)

})
