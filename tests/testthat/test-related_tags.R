context("test-related_tags")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  tag_test <- related_tags(tag = "tree")

  expect_is(tag_test, "character")
})

test_that("invalid API keys fails correctly", {
  # if(file.exists("api_key.txt")) { file.remove("api_key.txt") }
  # expect_error(related_tags(tag = "tree"), "Enter API key or save using the
  # save_key function")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(related_tags(tag = "tree"))
})
