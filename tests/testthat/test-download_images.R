context("test-download_images")

test_that("fails correctly", {
  expect_error(download_images(), "provide a photo id")
})


test_that("output is correct", {
  skip_on_travis()
  skip_on_cran()

  download_test <- download_images(photo_id = 47259127482, saveDir = "test_images", api_key = "3863b3b3d95341cff763f05e989012b7")

  expect_equal(file.exists(".\\test_images\\47259127482_05d7096ed3_o.jpg"), TRUE)


  file.remove(".\\test_images\\47259127482_05d7096ed3_o.jpg")
 })


