context("test-get_photoinfo")

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  photo_test <- get_photoinfo(photo_id = 47259127482)

  expect_is(photo_test, "data.frame")
})

test_that("wrong photo_id fails is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  expect_error(get_photoinfo(photo_id = 1))
})

test_that("invalid API keys fails correctly", {


  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(get_exif(photo_id = 47259127482),
               "Invalid API Key: correct this in photosearcher_key.sysdata")
})
