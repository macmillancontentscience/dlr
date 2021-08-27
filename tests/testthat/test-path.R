test_that("Can determine app dir.", {
  expect_identical(
    app_cache_dir("testing"),
    normalizePath(
      rappdirs::user_cache_dir(appname = "testing"),
      mustWork = FALSE
    )
  )

  old_option <- options(testing.dir = "/fake/path")
  on.exit(options(old_option))
  expect_identical(
    app_cache_dir("testing"),
    normalizePath(
      "/fake/path",
      mustWork = FALSE
    )
  )
})

test_that("Can construct processed filenames.", {
  # I only directly test this for a URL because the exact output for a file
  # changes from user to user, since the file is (rightly, I think) based on the
  # absolute path to that file.
  url_path <- "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co"

  test_result <- construct_processed_filename(
    source_path = url_path
  )
  expect_s3_class(test_result, c("fs_path", "character"))
  expect_identical(
    as.character(test_result),
    "owqxojjiphaypjmlxldsp566lck7co.4cdfc24e"
  )

  test_result <- construct_processed_filename(
    source_path = url_path,
    extension = "csv"
  )
  expect_s3_class(test_result, c("fs_path", "character"))
  expect_identical(
    as.character(test_result),
    "owqxojjiphaypjmlxldsp566lck7co.4cdfc24e.csv"
  )

  # Make sure weird corner cases for extensions are handled.
  expect_identical(
    construct_processed_filename(
      source_path = url_path,
      extension = NULL
    ),
    construct_processed_filename(
      source_path = url_path,
      extension = ""
    )
  )


  # The cache version is a relatively simple wrapper. Make sure it wraps as
  # expected. One slightly surprising aspect is that the class changes, since
  # it's normalized, but I want to keep the normalization and don't want to
  # double-wrap it.
  expect_identical(
    construct_cached_file_path(
      source_path = url_path,
      appname = "myApp"
    ),
    normalizePath(
      fs::path(
        app_cache_dir(appname = "myApp"),
        construct_processed_filename(source_path = url_path)
      ),
      mustWork = FALSE
    )
  )
})
