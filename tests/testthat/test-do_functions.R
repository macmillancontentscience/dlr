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

test_that("Corner cases work.", {
  expect_identical(
    .do_function(
      do_f = NULL,
      default_f = mean,
      main_args = list(1:10)
    ),
    mean(1:10)
  )
})

test_that("set_timeout works.", {
  old_option_value <- getOption("timeout")
  set_option <- set_timeout()
  expect_identical(
    set_option$timeout,
    old_option_value
  )
  expect_identical(
    getOption("timeout"),
    600L
  )
  set_timeout(10000L)
  expect_identical(
    getOption("timeout"),
    10000L
  )
  options(set_option)
})

test_that(".warn_timeout works.", {
  old_options <- options(
    timeout = 60L,
    dlr_timeout_warned = NULL
  )
  expect_warning(
    .warn_timeout(),
    "Your timeout is set to 60 seconds"
  )
  options(
    timeout = 100L,
    dlr_timeout_warned = FALSE
  )
  expect_warning(
    .warn_timeout(),
    "Your timeout is set to 100 seconds"
  )
  expect_warning(
    .warn_timeout(),
    NA
  )
  options(
    timeout = 1000L,
    dlr_timeout_warned = NULL
  )
  expect_warning(
    .warn_timeout(),
    NA
  )
  options(old_options)
})
