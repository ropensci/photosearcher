context("test-related_terms")

test_that("fails correctly", {
  expect_error(related_terms(), "provide a term")
})
