context("test-photo_search")

test_that("fails correctly", {
  expect_error(photo_search(mindate = NULL))
  expect_error(photo_search(maxdate = NULL))
})


test_that("output is correct", {
  skip_on_cran()

  tree_test <- photo_search(text = "tree", api_key = test_key)
  expect_is(tree_test, "data.frame")
  expect_equal(ncol(tree_test), 57)

  bbox_test <- photo_search(bbox = "-140.625000,-47.517201,167.695313,69.162558", api_key = test_key)
  expect_is(bbox_test, "data.frame")
  expect_equal(ncol(bbox_test), 57)

  large_search <- photo_search(mindate = "2018-12-20", maxdate = "2019-01-01", text = "lake", api_key = test_key)
  expect_is(large_search, "data.frame")
  expect_equal(ncol(large_search), 57)
})
