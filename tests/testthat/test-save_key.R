context("test save_key")

test_that("fails correctly", {
  expect_error(save_key(), "Provide a valid API key: create an API key at https://www.flickr.com/services/apps/create/apply\n")
})

test_that("invalid key is stopped", {
    skip_on_cran()
    expect_error(save_key(api_key = "asduad"), "Invalid API Key (Key has invalid format)")
})

test_that("output is correct", {
  skip_on_travis()
  skip_on_cran()

  key_test <- save_key(api_key = test_key)

  expect_equal(file.exists("api_key.rda"), TRUE)

  file.remove("api_key.rda")
})
