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

#' Read or Process a File
#'
#' Often, a file must be processed before being usable in R. It can be useful to
#' save the processed contents of that file in a standard format, such as RDS,
#' so that the file does not need to be processed the next time it is loaded.
#'
#' @param source_path Character scalar; the path to the raw file. Paths starting
#'   with `http://`, `https://`, `ftp://`, or `ftps://` will be downloaded to a
#'   temp file if the processed version is not already available.
#' @param target_path Character scalar; the path where the processed version of
#'   the file should be stored.
#' @param process_f A function or one-sided formula to use to process the source
#'   file. `source_path` will be passed as the first argument to this function.
#'   Defaults to `read_f`.
#' @param process_args An optional list of additional arguments to `process_f`.
#' @param read_f A function or one-sided formula to use to read the processed
#'   file. `target_path` will be passed as the first argument to this function.
#'   Defaults to [base::readRDS()].
#' @param read_args An optional list of additional arguments to `read_f`.
#' @param write_f A function or one-sided formula to use to save the processed
#'   file. The processed object will be passed as the first argument to this
#'   function, and `target_path` will be passed as the second argument. Defaults
#'   to [base::saveRDS()].
#' @param write_args An optional list of additional arguments to `write_f`.
#' @param force_process A logical scalar indicating whether we should process
#'   the source file even if the target already exists. This can be particularly
#'   useful if you wish to redownload a file.
#'
#' @return The processed object.
#' @export
#' @examples
#' if (interactive()) {
#'   temp_filename <- tempfile()
#'   austin_smoke_free <- read_or_process(
#'     "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     target_path = temp_filename,
#'     process_f = read.csv
#'   )
#'   head(austin_smoke_free)
#' }
#'
#' # Calling the function a second time gives the result instantly.
#' if (interactive()) {
#'   austin_smoke_free <- read_or_process(
#'     "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     target_path = temp_filename,
#'     process_f = read.csv
#'   )
#'   head(austin_smoke_free)
#' }
#'
#' if (interactive()) {
#'   # Remove the generated file.
#'   unlink(temp_filename)
#' }
read_or_process <- function(source_path,
                            target_path,
                            process_f = readRDS,
                            process_args = NULL,
                            read_f = readRDS,
                            read_args = NULL,
                            write_f = saveRDS,
                            write_args = NULL,
                            force_process = FALSE) {
  if (xor(missing(read_f), missing(write_f))) {
    cli::cli_abort(
      message = c(
        "read_f and write_f must be a matched pair.",
        i = "Please specify both read_f and write_f, or neither."
      ),
      class = "read_write_mismatch"
    )
  }

  # Things get weird if the functions are NULL, so let's make sure they aren't,
  # even if someone tried to be weird.
  read_f <- read_f %||% readRDS
  process_f <- process_f %||% readRDS
  write_f <- write_f %||% saveRDS

  # If source_path is a url, we do some magic to get everything to behave.
  if (.is_url(source_path)) {
    # nocov start; mocking this is complicated so I'm testing manually, at least
    # for now.
    process_f <- .download_then(process_f)
    process_args <- c(list(url = source_path), process_args)
    source_path <- tempfile()
    # nocov end
  }

  target_path <- fs::path_norm(target_path)

  # fs::file_exists provides better explanations of why things failed than base
  # file.exists.
  if (force_process || !fs::file_exists(target_path)) {
    processed_object <- .process_and_write(
      source_path = source_path,
      target_path = target_path,
      process_f = process_f,
      process_args = process_args,
      write_f = write_f,
      write_args = write_args
    )
  } else {
    # Read the already-processed file.
    processed_object <- .read_file(
      path = target_path,
      read_f = read_f,
      read_args = read_args
    )
  }

  return(processed_object)
}

#' Read or Cache a File
#'
#' This function wraps [read_or_process()], specifying an app's cache directory
#' as the target directory.
#'
#' @inheritParams read_or_process
#' @inheritParams app_cache_dir
#' @param filename Character; an optional filename for the cached version of the
#'   file. By default, a filename is constructed using
#'   [construct_processed_filename()].
#'
#' @return The processed object.
#' @export
#' @examples
#' if (interactive()) {
#'   austin_smoke_free <- read_or_cache(
#'     "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     appname = "dlr",
#'     process_f = read.csv
#'   )
#'   head(austin_smoke_free)
#' }
#'
#' if (interactive()) {
#'   # Calling the function a second time gives the result instantly.
#'   austin_smoke_free <- read_or_cache(
#'     "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     appname = "dlr",
#'     process_f = read.csv
#'   )
#'   head(austin_smoke_free)
#' }
#'
#' if (interactive()) {
#'   # Remove the generated file.
#'   unlink(
#'     construct_cached_file_path(
#'       "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co"
#'     )
#'   )
#' }
read_or_cache <- function(source_path,
                          appname,
                          filename = construct_processed_filename(source_path),
                          process_f = readRDS,
                          process_args = NULL,
                          read_f = readRDS,
                          read_args = NULL,
                          write_f = saveRDS,
                          write_args = NULL,
                          force_process = FALSE) {
  # Protect against NULL filename.
  if (is.null(filename)) {
    filename <- construct_processed_filename(source_path)
  }

  target_path <- fs::path(
    app_cache_dir(appname),
    filename
  )

  return(
    read_or_process(
      source_path = source_path,
      target_path = target_path,
      read_f = read_f,
      read_args = read_args,
      process_f = process_f,
      process_args = process_args,
      write_f = write_f,
      write_args = write_args,
      force_process = force_process
    )
  )
}

#' Process a File if Necessary
#'
#' Sometimes you just need to get a processed file to a particular location,
#' without reading the data. For example, you might need to download a lookup
#' table used by various functions in a package, independent of a particular
#' function call that needs the data. This function does the processing if it
#' hasn't already been done.
#'
#' @inheritParams read_or_process
#'
#' @return The normalized `target_path`.
#' @export
#' @examples
#' if (interactive()) {
#'   temp_filename <- tempfile()
#'   maybe_process(
#'     "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     target_path = temp_filename,
#'     process_f = read.csv
#'   )
#'
#'   unlink(temp_filename)
#' }
maybe_process <- function(source_path,
                          target_path,
                          process_f = readRDS,
                          process_args = NULL,
                          write_f = saveRDS,
                          write_args = NULL,
                          force_process = FALSE) {
  # This is cognate to read_or_process, but it returns the normalized
  # target_path. It's called for the side effect of processing or downloading or
  # whatever. Very similar code but not QUITE identical since it doesn't read.

  # Things get weird if the functions are NULL, so let's make sure they aren't,
  # even if someone tried to be weird.
  process_f <- process_f %||% readRDS
  write_f <- write_f %||% saveRDS

  # If source_path is a url, we do some magic to get everything to behave.
  if (.is_url(source_path)) { # nocov start; I test this stuff more directly
    process_f <- .download_then(process_f)
    process_args <- c(list(url = source_path), process_args)
    source_path <- tempfile()
  } # nocov end

  target_path <- fs::path_norm(target_path)

  # fs::file_exists provides better explanations of why things failed than base
  # file.exists.
  if (force_process || !fs::file_exists(target_path)) {
    .process_and_write(
      source_path = source_path,
      target_path = target_path,
      process_f = process_f,
      process_args = process_args,
      write_f = write_f,
      write_args = write_args
    )
  }

  return(target_path)
}

#' Cache a File if Necessary
#'
#' This function wraps [maybe_process()], specifying the app's cache directory.
#'
#' @inheritParams read_or_cache
#'
#' @return The normalized `target_path`.
#' @export
#' @examples
#' if (interactive()) {
#'   target_path <- maybe_cache(
#'     "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     appname = "dlr",
#'     process_f = read.csv
#'   )
#'   target_path
#'
#'   unlink(target_path)
#' }
maybe_cache <- function(source_path,
                        appname,
                        filename = construct_processed_filename(source_path),
                        process_f = readRDS,
                        process_args = NULL,
                        write_f = saveRDS,
                        write_args = NULL,
                        force_process = FALSE) {
  # This is cognate to read_or_cache. It wraps maybe_process in much the same
  # way read_or_cache wraps read_or_process.

  # Protect against NULL filename.
  if (is.null(filename)) {
    filename <- construct_processed_filename(source_path = source_path)
  }

  target_path <- fs::path(
    app_cache_dir(appname),
    filename
  )

  return(
    maybe_process(
      source_path = source_path,
      target_path = target_path,
      process_f = process_f,
      process_args = process_args,
      write_f = write_f,
      write_args = write_args,
      force_process = force_process
    )
  )
}
