# Copyright 2021 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

test_that("Can determine app dir.", {
  expect_identical(
    app_cache_dir("testing"),
    fs::path_norm(rappdirs::user_cache_dir(appname = "testing"))
  )

  old_option <- options(testing.dir = "/fake/path")
  on.exit(options(old_option))
  expect_identical(
    app_cache_dir("testing"),
    fs::path_norm("/fake/path")
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
    fs::path_norm(
      fs::path(
        app_cache_dir(appname = "myApp"),
        construct_processed_filename(source_path = url_path)
      )
    )
  )
})
