context("test-download_images")

test_that("fails correctly", {
  expect_error(download_images(), "provide a photo id")
})


test_that("output is correct", {
  skip_on_travis()
  skip_on_cran()

  download_test <- download_images(photo_id = 47259127482, saveDir = "test_images", api_key = test_key)

  expect_equal(file.exists(".\\test_images\\47259127482_05d7096ed3_o.jpg"), TRUE)

  file.remove(".\\test_images\\47259127482_05d7096ed3_o.jpg")
})

test_that("warnings are given", {
  expect_warning(download_images(photo_id = 46556758351, saveDir = "test_images", api_key = test_key), "No permission to download image 46556758351")
})
