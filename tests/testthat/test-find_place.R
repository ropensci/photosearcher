context("test-find_place")

test_that("fails correctly", {
  expect_error(find_place(), "provide a place")
})

test_that("output is correct", {
  place_test <- find_place(place = "france", api_key = "3863b3b3d95341cff763f05e989012b7")

  expect_is(place_test, "data.frame")
  expect_equal(ncol(place_test), 9)

})
