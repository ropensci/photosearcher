context("test-photo_search")

test_that("fails correctly", {
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  expect_error(photo_search(mindate_taken = NULL))
  expect_error(photo_search(maxdate_taken = NULL))

  skip_on_cran()

  expect_error(photo_search(bbox = "324134,12341341,123413241,312412"),
               "Not a valid bounding box")

  #whilst flickr services are down
  expect_error(photo_search(woe_id = "35356"),
               "Flickr location services are down")

  #no photographs meet criteria
  expect_error(photo_search(tags = c("big", "dog", "mad", "ship", "hot", "old"),
                            tags_any = FALSE))

  expect_error(photo_search(mindate_taken = "2017-01-01",
                            user_id = "155421853@N05"))

})

test_that("bbox + woe_id fails correctly", {
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  expect_error(
    photo_search(bbox = "-7.86,54.62,-1.0,58.83", woe_id = 12578048),
    "Specify search location as only one of: woe_id, bbox or sf_layer."
  )
})

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  tree_test <- photo_search(text = "tree",
                            maxdate_taken = "2019-01-02")
  expect_is(tree_test, "data.frame")
  expect_equal(ncol(tree_test), 59)

  bbox_test <- photo_search(
    bbox = "-140.625000,-47.517201,167.695313,69.162558",
    mindate_taken = "2019-01-01 16:00:00",
    maxdate_taken = "2019-01-01 23:00:00")
  expect_is(bbox_test, "data.frame")
  expect_equal(ncol(bbox_test), 59)


  date_test <- photo_search(mindate_uploaded = "2019-01-01",
                            maxdate_uploaded = "2019-02-01",
                            text = "lake")
  expect_is(date_test, "data.frame")
  expect_equal(ncol(date_test), 59)

  skip("Flickr location services are down")
  woeid_test <- photo_search(woe_id = 2347568)
  expect_is(woeid_test, "data.frame")
  expect_equal(ncol(woeid_test), 59)
})

test_that("shape files work", {

  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  national_parks = sf::st_read(system.file("shape/National_Parks_England.shp",
                                           package="photosearcher"))

  expect_warning(shape_test <- photo_search(mindate_taken = "2018-12-20",
                             sf_layer = national_parks,
                             text = "walk"))

  expect_is(shape_test, "data.frame")
  expect_equal(ncol(shape_test), 70)
  rm(national_parks)

})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(photo_search())
})
