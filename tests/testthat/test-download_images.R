context("test-download_images")

test_that("output is correct", {
  skip_on_cran()
  tempD <- tempdir()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  download_test <- download_images(photo_id = 47259127482,
                                   saveDir = tempD)

  expect_equal(file.exists(file.path(tempD, '47259127482_05d7096ed3_o.jpg')), TRUE)

  file.remove(file.path(tempD, '47259127482_05d7096ed3_o.jpg'))
})

test_that("warnings are given", {
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  expect_warning(download_images(photo_id = 46556758351, saveDir = tempD), "No permission to download image 46556758351")
})

test_that("invalid API keys fails correctly", {

  # if(file.exists("api_key.txt")) {
  #   file.remove("api_key.txt")
  # }
  #
  # expect_error(download_images(photo_id = 47259127482), "Visit https://www.flickr.com/services/apps/create/ to create an API key and save in api_key.txt")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(download_images(photo_id = 47259127482), "Invalid API Key: correct this in api_key.txt")
})
