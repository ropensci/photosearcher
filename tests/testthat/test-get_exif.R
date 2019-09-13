context("test-get_exif")

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  exif_test <- get_exif(photo_id = 47259127482)

  expect_is(exif_test, "data.frame")
  expect_equal(ncol(exif_test), 85)

})

test_that("wrong photo_id fails is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  expect_error(get_exif(photo_id = 1111111111111111111111111111),
               "Photo not found")
})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(get_exif(photo_id = 47259127482),
               "Invalid API Key: correct this in photosearcher_key.sysdata")
})
