test_that("Can determine app dir.", {
  expect_identical(
    app_cache_dir("testing"),
    normalizePath(
      rappdirs::user_cache_dir(appname = "testing"),
      mustWork = FALSE
    )
  )

  old_option <- options(testing.dir = "/fake/path")
  expect_identical(
    app_cache_dir("testing"),
    normalizePath(
      "/fake/path",
      mustWork = FALSE
    )
  )
  options(old_option)
})
