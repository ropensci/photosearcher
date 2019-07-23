context("test-get_exif")

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  exif_test <- get_exif(photo_id = 47259127482)

  expect_is(exif_test, "data.frame")
  expect_equal(ncol(exif_test), 85)

})

test_that("wrong photo_id fails is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  expect_error(get_exif(photo_id = 1111111111111111111111111111),
               "Photo not found")
})

test_that("invalid API keys fails correctly", {

  # if(file.exists("api_key.txt")) { file.remove("api_key.txt") }
  #
  # expect_error(download_images(photo_id = 47259127482), "Visit
  # https://www.flickr.com/services/apps/create/ to create an API key and save
  # in api_key.txt")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(get_exif(photo_id = 47259127482),
               "Invalid API Key: correct this in api_key.txt")
})
