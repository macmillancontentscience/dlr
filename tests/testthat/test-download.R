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

# We're going to assume download.file works, so really we just need to test the
# overall idea of the processing. This is a good case for mocking!

test_that("downloading to path works", {
  on.exit(
    unlink(
      fs::path(tempdir(), "sample.txt")
    )
  )
  stubbed_download <- function(...) {
    message("Downloading.")
    writeLines(
      "This is a sample file to pretend to download.",
      con = fs::path(tempdir(), "sample.txt")
    )
  }
  mockery::stub(
    where = download_path,
    what = "utils::download.file",
    how = stubbed_download
  )

  expect_message(
    test_return <- download_path(
      url = "https:://fakesite.com/sample.txt",
      path = tempdir()
    ),
    "Downloading."
  )
  expect_identical(
    test_return,
    fs::path(tempdir(), "sample.txt")
  )

  # The mock downloader only messages if the file doesn't exist.
  expect_message(
    download_path(
      url = "https:://fakesite.com/sample.txt",
      path = tempdir()
    ),
    NA
  )

  # Let's try a processor that does something else.
  test_process_f <- function(tempy, really, ...) {
    tempy <- normalizePath(tempy, mustWork = FALSE)
    really <- normalizePath(really, mustWork = FALSE)
    message(
      "Copying from ", tempy, " to ", really
    )
  }

  expect_message(
    download_path(
      url = "https:://fakesite.com/sample.txt",
      path = tempdir(),
      filename = "process.txt",
      process_f = test_process_f
    ),
    regexp = "Copying from .+\\.txt to .+process\\.txt"
  )
})
