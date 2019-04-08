context("test-related_terms")

test_that("output is data.frame", {
  api_key <- "3863b3b3d95341cff763f05e989012b7"
  df <- related_terms(term = "hiking")
  expect_is(df, "data.frame")
})
