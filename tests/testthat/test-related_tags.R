context("test-related_tags")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  tag_test <- related_tags(tag = "tree")

  expect_is(tag_test, "character")
})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(related_tags(tag = "tree"))
})
