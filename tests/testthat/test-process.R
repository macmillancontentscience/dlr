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

test_that("The maybe functions work.", {
  # Set up a file to process, and a place for it to go.
  test_source <- tempfile()
  test_target <- tempfile()
  on.exit(unlink(test_source))
  on.exit(unlink(test_target), add = TRUE)

  simple_df <- data.frame(
    a = 1:10,
    b = letters[1:10],
    c = 11:20
  )

  write.csv(simple_df, test_source, row.names = FALSE)

  simple_processor <- function(source_path) {
    head(read.csv(source_path), 1)
  }

  test_result <- maybe_process(
    source_path = test_source,
    target_path = test_target,
    process_f = simple_processor,
    write_f = saveRDS
  )

  # Make sure these are seen as the same on all systems.
  expect_identical(
    fs::path_norm(test_result),
    fs::path_norm(test_target)
  )

  test_result <- readRDS(test_target)
  expect_identical(
    test_result,
    head(simple_df, 1)
  )

  old_option <- options(testing.dir = tempdir())
  on.exit(options(old_option), add = TRUE)

  auto_filename <- maybe_cache(
    source_path = test_source,
    appname = "testing",
    process_f = simple_processor,
    write_f = saveRDS
  )
  on.exit(unlink(auto_filename), add = TRUE)

  # Make sure auto_filename is in tempdir, but otherwise don't worry about it.
  expect_true(
    startsWith(auto_filename, normalizePath(tempdir()))
  )

  test_result <- readRDS(auto_filename)
  expect_identical(
    test_result,
    head(simple_df, 1)
  )

  # It should be the same if the user explicitly sets the filename to NULL.
  null_auto_filename <- maybe_cache(
    source_path = test_source,
    appname = "testing",
    filename = NULL,
    process_f = simple_processor,
    write_f = saveRDS
  )
  expect_identical(null_auto_filename, auto_filename)
})

test_that("Can read-or-cache files.", {
  # This is very similar to the maybe functions, but I'm keeping it separate
  # because of idiosyncracies.

  # Set up a file to process, and a place for it to go.
  test_source <- tempfile()

  simple_df <- data.frame(
    a = 1:10,
    b = letters[1:10],
    c = 11:20
  )

  write.csv(simple_df, test_source, row.names = FALSE)

  simple_processor <- function(source_path) {
    head(read.csv(source_path), 1)
  }

  old_option <- options(testing.dir = tempdir())
  on.exit(options(old_option), add = TRUE)

  # Log the filename we expect the function to use; we don't actually care what
  # this is, but I want to clean up, so let's get what the system is using.
  auto_filename <- construct_cached_file_path(
    source_path = test_source,
    appname = "testing"
  )
  on.exit(unlink(auto_filename), add = TRUE)

  test_result <- read_or_cache(
    source_path = test_source,
    appname = "testing",
    process_f = simple_processor,
    write_f = saveRDS
  )

  expect_identical(
    test_result,
    head(simple_df, 1)
  )

  # I guess we should make sure it's where we think.
  expect_identical(
    test_result,
    readRDS(auto_filename)
  )

  # Run it again to make sure the result is the same. We'll check that NULL
  # filename gets repaired properly at the same time.
  test_result <- read_or_cache(
    source_path = test_source,
    appname = "testing",
    filename = NULL,
    process_f = simple_processor,
    write_f = saveRDS
  )
  expect_identical(
    test_result,
    head(simple_df, 1)
  )

  # Make sure we error properly if they misunderstand read/write.
  expect_error(
    read_or_process(
      source_path = "source",
      target_path = "target",
      read_f = something_special
    ),
    class = "read_write_mismatch"
  )
  expect_error(
    read_or_process(
      source_path = "source",
      target_path = "target",
      write_f = something_special
    ),
    class = "read_write_mismatch"
  )
})
