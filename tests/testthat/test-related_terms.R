context("test-related_terms")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  term_test <- related_terms(term = "tree")

  expect_is(term_test, "character")
})

test_that("invalid API keys fails correctly", {
  # if(file.exists("api_key.txt")) {
  #   file.remove("api_key.txt")
  # }
  # expect_error(related_terms(term = "tree"), "Enter API key or save using the save_key function")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(related_terms(term = "tree"))
})
