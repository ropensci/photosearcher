context("test-location_tags")

test_that("output is correct", {
  skip_on_cran()
  write.table("6a2ac025703c4b98aae141842eae8b1d", file = "api_key.txt")
  loc_test <- location_tags(woe_id = 35356)

  expect_is(loc_test, "character")
  expect_equal(length(loc_test), 100)
})

test_that("invalid API keys fails correctly", {
  # if(file.exists("api_key.txt")) {
  #   file.remove("api_key.txt")
  # }
  # expect_error(location_tags(woe_id = 35356), "Enter API key or save using the save_key function")

  skip_on_cran()
  write.table("notarealkey", file = "api_key.txt")
  expect_error(location_tags(woe_id = 35356))
})
