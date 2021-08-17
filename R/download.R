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

#' Download to a Path
#'
#' This function downloads a file to a given path if that file does not already
#' exist. This is mostly a convenience wrapper around
#' \code{\link[utils]{download.file}}, with checks to make sure the path exists
#' and the file hasn't already been downloaded.
#'
#' @param url Character; the location of the file on the internet.
#' @param path Character; the local directory to which the file should be
#'   written.
#' @param filename Character; an optional filename for the file (by default, the
#'   filename of the remote file is used).
#' @param process_f A function or one-sided formula to load, process, and save
#'   the downloaded files. The temporary path to the downloaded file is passed
#'   as the first argument to this function, and the target path is passed as
#'   the second argument. See examples.
#' @param redownload Logical; should the file be redownloaded if it already
#'   exists locally?
#' @param ... Additional arguments passed to \code{process_f}.
#'
#' @return The full path to the local file.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   download_path(
#'     url = "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     path = tempdir()
#'   )
#'
#'   # Or process before saving.
#'   csv_to_rds <- function(temp_file, final_file) {
#'     df <- read.csv(
#'       temp_file,
#'       header = TRUE,
#'       stringsAsFactors = FALSE
#'     )
#'     saveRDS(df, final_file)
#'   }
#'
#'   download_path(
#'     url = "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     path = tempdir(),
#'     filename = "austin_smoke_free.rds",
#'     process_f = csv_to_rds
#'   )
#' }
#' }
download_path <- function(url,
                          path,
                          filename = fs::path_file(url),
                          process_f = NULL,
                          redownload = FALSE,
                          ...) {
  fs::dir_create(path)
  path <- fs::path(path, filename)

  if (!file.exists(path) || redownload) {
    if (is.null(process_f)) {
      utils::download.file(url, path, mode = "wb")
    } else {
      # Allow for purrr-style formula syntax.
      process_f <- rlang::as_function(process_f)

      # Save the unprocessed file to temp.
      temp_path <- tempfile(
        fileext = paste0(
          ".",
          fs::path_ext(fs::path_file(url))
        )
      )
      on.exit(unlink(temp_path))
      utils::download.file(url, temp_path, mode = "wb")

      # Do whatever the processor does.
      process_f(temp_path, path, ...)
    }
  }

  return(path)
}

#' Download to App Cache Dir
#'
#' Download files to an application cache dir, using platform conventions as
#' defined in \code{\link[rappdirs]{user_cache_dir}}.
#'
#' @inheritParams download_path
#' @param appname Character; the name of the application that will "own" this
#'   file, such as the name of a package.
#'
#' @return The full path to the local file.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   download_cache(
#'     url = "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     appname = "dlr"
#'   )
#'
#'   # Or process before saving.
#'   csv_to_rds <- function(temp_file, final_file) {
#'     df <- read.csv(
#'       temp_file,
#'       header = TRUE,
#'       stringsAsFactors = FALSE
#'     )
#'     saveRDS(df, final_file)
#'   }
#'
#'   download_cache(
#'     url = "https://query.data.world/s/owqxojjiphaypjmlxldsp566lck7co",
#'     appname = "dlr",
#'     filename = "austin_smoke_free.rds",
#'     process_f = csv_to_rds
#'   )
#' }
#' }
download_cache <- function(url,
                           appname,
                           filename = fs::path_file(url),
                           process_f = NULL,
                           redownload = FALSE,
                           ...) {
  # The only way this differs from download_path is that it uses an automatic
  # path, and that makes it hard to test. Relying on manual tests for this.
  path <- app_cache_dir(appname) # nocov start
  return(
    download_path(
      url = url,
      path = path,
      filename = filename,
      process_f = process_f,
      redownload = redownload,
      ...
    )
  ) # nocov end
}
