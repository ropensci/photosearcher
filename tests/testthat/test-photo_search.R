context("test-photo_search")

test_that("fails correctly", {
  expect_error(photo_search(mindate = NULL))
  expect_error(photo_search(maxdate = NULL))
})


test_that("output is correct", {
  tree_test <- photo_search(text = "tree", api_key = "3863b3b3d95341cff763f05e989012b7")

  expect_is(tree_test, "data.frame")
  expect_equal(ncol(tree_test), 57)

})





