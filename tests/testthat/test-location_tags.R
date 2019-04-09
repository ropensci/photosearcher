context("test-location_tags")

test_that("fails correctly", {
  expect_error(location_tags(), "provide woe_id")
})

