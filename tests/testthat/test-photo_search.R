context("test-photo_search")

test_that("output is data.frame", {
  d <- photo_search()
  expect_is(d, "data.frame")
})
