context("test-photo_search")

test_that("fails correctly", {
  expect_error(photo_search(mindate_taken = NULL))
  expect_error(photo_search(maxdate_taken = NULL))

  expect_error(photo_search(bbox = "324134,12341341,123413241,312412"),
               "Not a valid bounding box")

})

test_that("bbox + woe_id fails correctly", {
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  expect_error(
    photo_search(bbox = "-7.86,54.62,-1.0,58.83", woe_id = 12578048),
    "Specify search location as only one of: woe_id, bbox or sf_layer."
  )
})

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  tree_test <- photo_search(text = "tree")
  expect_is(tree_test, "data.frame")
  expect_equal(ncol(tree_test), 57)

  bbox_test <- photo_search(
    bbox = "-140.625000,-47.517201,167.695313,69.162558")
  expect_is(bbox_test, "data.frame")
  expect_equal(ncol(bbox_test), 57)

  user_test <- photo_search(mindate_taken = "2017-01-01",
                            user_id = "33816646@N06")
  expect_is(user_test, "data.frame")

  date_test <- photo_search(mindate_uploaded = "2019-01-01",
                            maxdate_uploaded = "2019-02-01",
                            text = "lake")
  expect_is(date_test, "data.frame")
  expect_equal(ncol(date_test), 57)

  large_search <- photo_search(mindate_taken = "2018-12-20",
                               maxdate_taken = "2019-01-01",
                               text = "lake")
  expect_is(large_search, "data.frame")
  expect_equal(ncol(large_search), 57)

  skip("Shape file doesn't load")
  shape_file <- sf::read_sf(
    ".\\tests\\testthat\\helper_shape_file\\National_Parks_England.shp")
  shape_test <- photo_search(sf_layer = shape_file)
  expect_is(shape_test, "data.frame")
  expect_equal(ncol(shape_test), 68)
  rm(shape_file)

  skip("Flickr location services are down")
  woeid_test <- photo_search(woe_id = 2347568)
  expect_is(woeid_test, "data.frame")
  expect_equal(ncol(woeid_test), 57)
})

test_that("invalid API keys fails correctly", {

  # if(file.exists("api_key.txt")) { file.remove("api_key.txt") }
  #
  # expect_error(photo_search(), "Visit
  # https://www.flickr.com/services/apps/create/ to create an API key and save
  # in api_key.txt")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(photo_search())
})
