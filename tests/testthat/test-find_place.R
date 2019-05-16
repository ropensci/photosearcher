context("test-find_place")

test_that("output is correct", {
  skip_on_cran()

  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  place_test <- find_place(place = "france")

  expect_is(place_test, "data.frame")
  expect_equal(ncol(place_test), 9)
})

test_that("invalid API keys fails correctly", {
  # if(file.exists("api_key.txt")) {
  #   file.remove("api_key.txt")
  # }
  # expect_error(find_place(place = "new york"), "Enter API key or save using the save_key function")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(find_place(place = "new york"))
})
