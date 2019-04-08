context("test-user_info")

test_that("output is data.frame", {
  api_key <- "3863b3b3d95341cff763f05e989012b7"
  df <- user_info(user_id = "33816646@N06")
  expect_is(df, "data.frame")
})

