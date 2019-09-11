context("test-download_images")

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  download_test <- download_images(photo_id = 47259127482,
                                   save_dir = "test_images")

  expect_is(download_test, "data.frame")
  expect_equal(ncol(download_test), 2)

  expect_equal(file.exists("test_images/47259127482_05d7096ed3_o.jpg"), TRUE)

  file.remove("test_images/47259127482_05d7096ed3_o.jpg")


})

test_that("height and width can be chosen", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  size_test <- download_images(photo_id = 47259127482,
                                   max_image_height = 1200,
                                   max_image_width = 1200,
                                   save_dir = "test_images")

  expect_is(size_test, "data.frame")
  expect_equal(ncol(size_test), 2)

  expect_equal(file.exists("test_images/47259127482_66561d03eb_b.jpg"), TRUE)

  file.remove("test_images/47259127482_66561d03eb_b.jpg")

})

test_that("if no photos match height and width, skip", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  size_test <- download_images(photo_id = 47259127482,
                               max_image_height = 1,
                               max_image_width = 1,
                               save_dir = "test_images")

  expect_is(size_test, "data.frame")
  expect_equal(ncol(size_test), 2)
})

test_that("if photo is not found, skip", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  notreal_test <- download_images(photo_id = "not a real photo",
                                  save_dir = "test_images")

  expect_is(notreal_test, "data.frame")
  expect_equal(ncol(notreal_test), 2)

})

test_that("if photo has no permission, skip", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  notreal_test <- download_images(photo_id = "1231231",
                                  save_dir = "test_images")

  expect_is(notreal_test, "data.frame")
  expect_equal(ncol(notreal_test), 2)

})


test_that("invalid API keys fails correctly", {

  # if(file.exists("api_key.txt")) { file.remove("api_key.txt") }
  #
  # expect_error(download_images(photo_id = 47259127482), "Visit
  # https://www.flickr.com/services/apps/create/ to create an API key and save
  # in api_key.txt")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(download_images(photo_id = 47259127482,
                               save_dir = "test_images"),
               "Invalid API Key: correct this in api_key.txt")
})
