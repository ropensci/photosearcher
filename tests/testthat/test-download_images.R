context("test-download_images")

test_that("fails correctly", {
  expect_error(download_images(), "provide a photo id")
})
