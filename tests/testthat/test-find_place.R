context("test-find_place")

test_that("fails correctly", {
  expect_error(find_place(), "provide a place")
})

test_that("output is correct", {
  skip_on_cran()

  place_test <- find_place(place = "france", api_key = test_key)

  expect_is(place_test, "data.frame")
  expect_equal(ncol(place_test), 9)
})

test_that("invalid API keys fails correctly", {
  expect_error(find_place(api_key = NULL, place = "new york"), "Enter API key or save using the save_key function")

  skip_on_cran()
  expect_error(find_place(api_key = "notarealkey", place = "new york"))
})
