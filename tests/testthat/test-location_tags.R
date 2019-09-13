context("test-location_tags")

test_that("output is correct", {
  skip_on_cran()
  skip("Flickr location services are down")
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  loc_test <- location_tags(woe_id = 35356)

  expect_is(loc_test, "character")
  expect_equal(length(loc_test), 100)
})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(location_tags(woe_id = 35356))
})
