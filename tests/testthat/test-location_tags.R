context("test-location_tags")

test_that("output is data.frame", {
  api_key <- "3863b3b3d95341cff763f05e989012b7"
  df <- location_tags(woe_id = 35356)
  expect_is(df, "data.frame")
})
