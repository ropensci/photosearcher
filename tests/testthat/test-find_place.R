context("test-find_place")

test_that("output is data.frame", {
  api_key <- "3863b3b3d95341cff763f05e989012b7"
  df <- find_place(place = "London")
  expect_is(df, "data.frame")
})
