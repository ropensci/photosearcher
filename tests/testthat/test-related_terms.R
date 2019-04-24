context("test-related_terms")

test_that("fails correctly", {
  expect_error(related_terms(), "provide a term")
})

test_that("output is correct", {
  term_test <- related_terms(term = "tree", api_key = "3863b3b3d95341cff763f05e989012b7")

  expect_is(term_test, "data.frame")

})
