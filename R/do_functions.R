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

#' Read/Process a File
#'
#' @param path Character scalar; the full path to the file.
#' @param read_f The function to use to read and process the file. The first
#'   argument to this function must be the path to the file.
#' @param read_args Additional arguments to `read_f`.
#'
#' @return The processed file.
#' @keywords internal
.read_file <- function(path,
                       read_f,
                       read_args) {
  # The "read" and "process" steps are really just two different "read" steps.
  # This function executes either one.
  return(
    .do_function(
      do_f = read_f,
      default_f = readRDS,
      main_args = list(path),
      extra_args = read_args
    )
  )
}

#' Write an Object
#'
#' @param object An R object.
#' @param target_path Character scalar; the full path to the file to write.
#' @param write_f The function to use to write the file. The first argument to
#'   this function must be the object, and the second argument must be the path
#'   to the file.
#' @param write_args Additional arguments to `write_f`.
#'
#' @return The return from the call to `write_f`.
#' @keywords internal
.write_object <- function(object,
                          target_path,
                          write_f,
                          write_args) {
  # Create the dir part of the path if necessary.
  target_dir <- dirname(target_path)
  fs::dir_create(target_dir, recurse = TRUE)

  # Use their function (or saveRDS if NULL) to write the file.
  return(
    .do_function(
      do_f = write_f,
      default_f = saveRDS,
      main_args = list(object, target_path),
      extra_args = write_args
    )
  )
}

#' Do a Function Call
#'
#' @param do_f The function to call.
#' @param default_f The function to call if `do_f` is `NULL`.
#' @param main_args A list containing the first argument(s) to `do_f`.
#' @param extra_args A list containing any additional arguments to `do_f`.
#'
#' @return The return from the call to `do_f`.
#' @keywords internal
.do_function <- function(do_f, default_f, main_args, extra_args = NULL) {
  if (is.null(do_f)) {
    do_f <- default_f
  } else {
    do_f <- rlang::as_function(do_f)
  }
  return(
    do.call(
      do_f,
      args = c(
        main_args,
        extra_args
      )
    )
  )
}

#' Process and Write a File
#'
#' @inheritParams read_or_process
#'
#' @return The processed object.
#' @keywords internal
.process_and_write <- function(source_path,
                               target_path,
                               process_f,
                               process_args,
                               write_f,
                               write_args) {
  # We intentionally do NOT validate that source_path exists, because various
  # tricks in process_f (such as those we do for .download_then) might
  # cause source_path to not yet exist.

  # Use the processor to read and process the raw file.
  processed_object <- .read_file(
    path = source_path,
    read_f = process_f,
    read_args = process_args
  )
  question <- paste0("Cache processed file at ", target_path, "?")
  if (!interactive() || utils::askYesNo(question)) {
    .write_object(
      object = processed_object,
      target_path = target_path,
      write_f = write_f,
      write_args = write_args
    )
  }

  return(processed_object)
}

#' Compose a Download-then-Process Function
#'
#' @param process_f A function or one-sided formula to use to read and process
#'   the raw file after it is downloaded. The path to the downloaded file will
#'   be passed as the first argument to this function.
#'
#' @return The processed data returned by `process_f`.
#' @keywords internal
.download_then <- function(process_f) {
  # nocov start; I tried testing this directly, but covr gets weird. It's tested
  # as part of manual download tests.
  process_f <- rlang::as_function(process_f)
  return(
    function(temp_path, url, ...) {
      # read_processed_file will also try to delete the downloaded file after
      # processing, but I want to make sure we catch it if there's an error.
      on.exit(unlink(temp_path))
      utils::download.file(url, temp_path, mode = "wb")
      return(process_f(temp_path, ...))
    }
  )
  # nocov end
}
