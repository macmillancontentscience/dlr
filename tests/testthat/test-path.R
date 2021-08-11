test_that("Can determine app dir.", {
  # Right now this function exactly duplicates rappdirs functionality. I'm
  # setting up this test for when we add additional options.
  expect_identical(
    app_cache_dir("testing"),
    rappdirs::user_cache_dir(appname = "testing")
  )
})
