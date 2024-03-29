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

#' Path to an App Cache Directory
#'
#' App cache directories can depend on the user's operating system and an
#' overall `R_USER_CACHE_DIR` environment variable. We also respect a per-app
#' option (`appname.dir`), and a per-app environment variable
#' (`APPNAME_CACHE_DIR`). This function returns the path that will be used for a
#' given app's cache.
#'
#' @param appname Character; the name of the application that will "own" the
#'   cache, such as the name of a package.
#'
#' @return The full path to the app's cache directory.
#' @export
#'
#' @examples
#' app_cache_dir("myApp")
app_cache_dir <- function(appname, verbose = interactive()) {
  cache_env <- Sys.getenv(paste0(toupper(appname), "_CACHE_DIR"))
  if (cache_env == "") cache_env <- NULL

  cache_dir <- getOption(paste0(appname, ".dir")) %||%
    cache_env %||%
    rappdirs::user_cache_dir(appname = appname)

  cache_dir <- fs::path_norm(cache_dir)

  # Potentially alert the user if there's a problem with the app dir.
  .cache_dir_messaging(
    appname = appname,
    cache_dir = cache_dir,
    message_style = "warning",
    verbose = verbose
  )

  return(cache_dir)
}

#' Set a Cache Directory for an App
#'
#' Override the default paths used by [app_cache_dir()].
#'
#' @inheritParams app_cache_dir
#' @param cache_dir Character scalar; a path to a cache directory.
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist. An option is
#'   also set so future calls to [app_cache_dir()] will respect the
#'   change.
#' @export
#'
#' @examplesIf interactive()
#' # Executing this function creates a cache directory.
#' set_app_cache_dir(appname = "dlr", cache_dir = "/my/cache/path")
set_app_cache_dir <- function(appname, cache_dir = NULL) {
  # Testing this creates directories on the user's system, so I am testing this
  # manually for now.

  # nocov start
  cache_dir <- cache_dir %||%
    app_cache_dir(appname = appname, verbose = FALSE)
  cache_dir <- fs::path_norm(cache_dir)

  if (!file.exists(cache_dir)) {
    fs::dir_create(cache_dir, recurse = TRUE)
  } else {
    .cache_dir_messaging(
      appname = appname,
      cache_dir = cache_dir,
      message_style = "abort",
      verbose = TRUE
    )
  }

  options(rlang::set_names(list(cache_dir), paste0(appname, ".dir")))

  return(cache_dir)
  # nocov end
}

#' Create a Cache Directory for an App
#'
#' Create the default path expected by [app_cache_dir()].
#'
#' @inheritParams app_cache_dir
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist.
#' @export
#'
#' @examplesIf interactive()
#' # Executing this function creates a cache directory.
#' create_app_cache_dir("dlr")
create_app_cache_dir <- function(appname) {
  # Testing this would create a directory in a default location, so I'm just
  # testing manually.
  return(set_app_cache_dir(appname)) # nocov
}

#' Reusable Path Permissions Messaging
#'
#' @inheritParams app_cache_dir
#' @param cache_dir The directory that will be used for caching.
#' @param message_style Whether to "abort" (default) or issue a "warning".
#' @param verbose Whether to issue the messages (included for easier usage in
#'   other functions).
#'
#' @return `TRUE` invisibly.
#' @keywords internal
.cache_dir_messaging <- function(appname,
                                 cache_dir,
                                 message_style = "abort",
                                 verbose) {
  if (verbose) {
    message_function <- if (isTRUE(message_style == "warning")) {
      cli::cli_warn
    } else {
      cli::cli_abort
    }
    if (!file.exists(cache_dir)) {
      message_function(
        message = c(
          glue::glue("{cache_dir} does not exist."),
          i = glue::glue("Create it with `create_app_cache_dir('{appname}')`")
        )
      )
    } else if (file.access(cache_dir, 4) != 0) { # nocov start
      # file.access returns 0 for success because it hates clean code. The user
      # has to have read permissions for this function. I can't figure out a
      # good way to test the case where the directory exists but this user
      # doesn't have read access to it.
      message_function(
        message = glue::glue("You do not have read access to {cache_dir}"),
        class = "dir_read_error"
      )
    } # nocov end
  }
  return(invisible(TRUE))
}

#' Construct Processed Filename
#'
#' Given the path to a file, construct a unique filename using the hash of the
#' path.
#'
#' @param source_path Character scalar; the full path to the source file.
#' @param extension Character scalar; an optional filename extension.
#'
#' @return A unique filename for a processed version of the file.
#'
#' @export
#' @examples
#' construct_processed_filename(
#'   source_path = "my/file.txt",
#'   extension = "rds"
#' )
construct_processed_filename <- function(source_path,
                                         extension = "") {
  just_name <- basename(source_path)
  dirpath <- dirname(source_path)

  # Hack to make this useful for both URLs and local files.
  if (!.is_url(source_path)) {
    dirpath <- fs::path_norm(source_path)
  }

  path_hash <- digest::digest(dirpath, algo = "xxhash32")

  # Deal with various ways someone might try to say "no extension".
  if (is.null(extension) || length(extension) == 0) extension <- ""

  # Stitch it all together.
  full_filename <- fs::path(
    paste(just_name, path_hash, sep = "."),
    ext = extension
  )

  return(full_filename)
}

#' Determine if Path is a URL
#'
#' @param source_path Character; the path to check.
#'
#' @return Logical indicating whether that looks like a url (`TRUE` if it starts
#'   with `http://`, `https://`, `ftp://`, or `ftps://`, `FALSE` otherwise).
#' @keywords internal
.is_url <- function(source_path) {
  grepl("^(ht|f)tps?://", source_path)
}

#' Construct Cache Path
#'
#' Construct the full path to the cached version of a file within a particular
#' app's cache, using the source path of the file to make sure the cache
#' filename is unique.
#'
#' @inheritParams construct_processed_filename
#' @inheritParams app_cache_dir
#'
#' @return The full path to the processed version of source_path in the app's
#'   cache directory.
#' @export
#'
#' @examples
#' construct_cached_file_path(
#'   source_path = "my/file.txt",
#'   appname = "dlr",
#'   extension = "rds"
#' )
construct_cached_file_path <- function(source_path,
                                       appname,
                                       extension = "") {
  return(
    fs::path_norm(
      fs::path(
        app_cache_dir(appname),
        construct_processed_filename(
          source_path = source_path,
          extension = extension
        )
      )
    )
  )
}
