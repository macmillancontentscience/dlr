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
#' @param redownload Logical; should the file be redownloaded if it already
#'   exists locally?
#'
#' @return The full path to the local file.
#' @export
#'
#' @examples
#' \donttest{
#' if(interactive()){
#'   download_path(
#'     url = "https://raw.githubusercontent.com/macmillancontentscience/dlr/main/README.Rmd",
#'     path = tempdir()
#'   )
#' }
#' }
download_path <- function(url, path, redownload = FALSE) {
  fs::dir_create(path)
  path <- fs::path(path, fs::path_file(url))

  if (!file.exists(path) || redownload) {
    utils::download.file(url, path, mode = "wb")
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
#' if(interactive()){
#'   download_cache(
#'     url = "https://raw.githubusercontent.com/macmillancontentscience/dlr/main/README.Rmd",
#'     appname = "dlr"
#'   )
#' }
#' }
download_cache <- function(url, appname, redownload = FALSE) {
  # The only way this differs from download_path is that it uses an automatic
  # path, and that makes it hard to test. Relying on manual tests for this.
  path <- rappdirs::user_cache_dir(appname) # nocov
  return(download_path(url, path, redownload)) # nocov
}
