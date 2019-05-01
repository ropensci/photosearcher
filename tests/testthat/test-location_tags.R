context("test-location_tags")

test_that("fails correctly", {
  expect_error(location_tags(), "provide woe_id")
})


test_that("output is correct", {
  skip_on_cran()

  loc_test <- location_tags(woe_id = 35356, api_key = test_key)

  expect_is(loc_test, "data.frame")
  expect_equal(nrow(loc_test), 100)
})

test_that("invalid API keys fails correctly", {
  expect_error(location_tags(api_key = NULL, woe_id = 35356), "Enter API key or save using the save_key function")

  skip_on_cran()
  expect_error(location_tags(api_key = "notarealkey", woe_id = 35356))
})
