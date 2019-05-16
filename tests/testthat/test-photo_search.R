context("test-photo_search")

test_that("fails correctly", {
  expect_error(photo_search(mindate = NULL))
  expect_error(photo_search(maxdate = NULL))
})

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  tree_test <- photo_search(text = "tree")
  expect_is(tree_test, "data.frame")
  expect_equal(ncol(tree_test), 57)

  bbox_test <- photo_search(bbox = "-140.625000,-47.517201,167.695313,69.162558")
  expect_is(bbox_test, "data.frame")
  expect_equal(ncol(bbox_test), 57)

  large_search <- photo_search(mindate = "2018-12-20", maxdate = "2019-01-01", text = "lake")
  expect_is(large_search, "data.frame")
  expect_equal(ncol(large_search), 57)
})

test_that("invalid API keys fails correctly", {

  # if(file.exists("api_key.txt")) {
  #   file.remove("api_key.txt")
  # }
  #
  # expect_error(photo_search(), "Visit https://www.flickr.com/services/apps/create/ to create an API key and save in api_key.txt")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(photo_search())
})
