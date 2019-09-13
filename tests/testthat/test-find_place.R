context("test-find_place")

test_that("output is correct", {
  skip_on_cran()
  skip("Flickr location services are down")

  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  place_test <- find_place(place = "france")

  expect_is(place_test, "data.frame")
  expect_equal(ncol(place_test), 9)
})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(find_place(place = "new york"))
})
