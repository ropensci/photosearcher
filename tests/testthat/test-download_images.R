context("test-download_images")

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  download_test <- download_images(photo_id = 47259127482,
                                   save_dir = ".")

  expect_is(download_test, "data.frame")
  expect_equal(ncol(download_test), 2)

  expect_equal(file.exists("./47259127482_05d7096ed3_o.jpg"), TRUE)

  file.remove("./47259127482_05d7096ed3_o.jpg")


})

test_that("height and width can be chosen", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  size_test <- download_images(photo_id = 47259127482,
                                   max_image_height = 1200,
                                   max_image_width = 1200,
                                   save_dir = ".")

  expect_is(size_test, "data.frame")
  expect_equal(ncol(size_test), 2)

  expect_equal(file.exists("./47259127482_66561d03eb_b.jpg"), TRUE)

  file.remove("./47259127482_66561d03eb_b.jpg")

})

test_that("if no photos match height and width, skip", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  size_test <- download_images(photo_id = 47259127482,
                               max_image_height = 1,
                               max_image_width = 1,
                               save_dir = ".")

  expect_is(size_test, "data.frame")
  expect_equal(ncol(size_test), 2)
})

test_that("if photo is not found, skip", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  notreal_test <- download_images(photo_id = "1",
                                  save_dir = ".")

  expect_is(notreal_test, "data.frame")
  expect_equal(ncol(notreal_test), 2)

})

test_that("if photo has no permission, skip", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")
  no_perm_test <- download_images(photo_id = "1231231",
                                  save_dir = ".")

  expect_is(no_perm_test, "data.frame")
  expect_equal(ncol(no_perm_test), 2)

})

#test overwrite status
test_that("overwrite files works", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  overwrite_test <- download_images(photo_id = 48704764812,
                               save_dir = ".")

  overwrite_false_test <- download_images(photo_id = c(48704764812,
                                                       48704764812),
                                     save_dir = ".",
                                     overwrite_file = FALSE)

  expect_is(overwrite_false_test, "data.frame")
  expect_equal(ncol(overwrite_false_test), 2)

  overwrite_true_test <- download_images(photo_id = c(48704764812,
                                                      48704764812),
                                          save_dir = ".",
                                          overwrite_file = TRUE)

  expect_is(overwrite_true_test, "data.frame")
  expect_equal(ncol(overwrite_true_test), 2)


})

#test errors
test_that("save_dir is needed", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d",
              file = "photosearcher_key.sysdata")

  expect_error(download_images(photo_id = 47259127482,
                               save_dir = "asjdnflajsndflja"),
               "Please supply a save directory")

  expect_error(download_images(photo_id = 47259127482,
                               save_dir = NULL),
               "invalid filename argument")

})

test_that("invalid API keys fails correctly", {

  skip_on_cran()
  write.table("notarealkey", file = "photosearcher_key.sysdata")
  expect_error(download_images(photo_id = 47259127482,
                               save_dir = "."),
               "Invalid API Key: correct this in photosearcher_key.sysdata")
})
