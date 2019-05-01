context("test-related_terms")

test_that("fails correctly", {
  expect_error(related_terms(), "provide a term")
})

test_that("output is correct", {
  skip_on_cran()

  term_test <- related_terms(term = "tree", api_key = test_key)

  expect_is(term_test, "data.frame")
})
