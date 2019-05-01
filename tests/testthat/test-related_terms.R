context("test-related_terms")

test_that("fails correctly", {
  expect_error(related_terms(), "provide a term")
})

test_that("output is correct", {
  skip_on_cran()

  term_test <- related_terms(term = "tree", api_key = test_key)

  expect_is(term_test, "data.frame")
})

test_that("invalid API keys fails correctly", {
  expect_error(related_terms(api_key = NULL, term = "tree"), "Enter API key or save using the save_key function")

  skip_on_cran()
  expect_error(related_terms(api_key = "notarealkey", term = "tree"))
})
