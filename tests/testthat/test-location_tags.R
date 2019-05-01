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
