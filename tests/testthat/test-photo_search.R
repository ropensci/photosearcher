context("test-photo_search")

test_that("fails correctly", {
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  expect_error(photo_search(mindate_taken = NULL))
  expect_error(photo_search(maxdate_taken = NULL))

  skip_on_cran()

  expect_error(photo_search(bbox = "123123123,123,1231231,123",
                            mindate_taken = "2019-01-01",
                            maxdate_taken = "2019-02-01"),
               "Not a valid bounding box")
})


test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  text_test <- photo_search(text = "tree",
                           mindate_taken = "2019-01-01",
                           maxdate_taken = "2019-01-02")
  expect_is(text_test, "data.frame")
  expect_equal(ncol(text_test), 62)

  bbox_test <- photo_search(
    bbox = "-140.625000,-47.517201,167.695313,69.162558",
    mindate_taken = "2019-01-01 16:00:00",
    maxdate_taken = "2019-01-01 23:00:00")
  expect_is(bbox_test, "data.frame")
  expect_equal(ncol(bbox_test), 62)


  date_test <- photo_search(mindate_uploaded = "2019-01-01",
                            maxdate_uploaded = "2019-03-01",
                            mindate_taken = "2019-01-01",
                            maxdate_taken = "2019-01-02",
                            text = "tree")
  expect_is(date_test, "data.frame")
  expect_equal(ncol(date_test), 62)

})

test_that("shape files work", {

  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  national_parks <- sf::st_read(system.file("shape/National_Parks_England.shp",
                                           package="photosearcher"))

  shape_test <- photo_search(mindate_taken = "2018-12-28",
                                            maxdate_taken = "2019-01-01",
                                            sf_layer = national_parks,
                                            text = "walk")

  expect_is(shape_test, "data.frame")
  expect_equal(ncol(shape_test), 64)
  rm(national_parks)

})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(photo_search())
})
