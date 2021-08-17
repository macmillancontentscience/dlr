#' Path to an App Cache Directory
#'
#' App cache directories can depend on the user's operating system and an
#' overall \code{R_USER_CACHE_DIR} environment variable. We also respect a
#' per-app option (\code{appname.dir}), and a per-app environment variable
#' (\code{APPNAME_CACHE_DIR}. This function returns the path that will be used
#' for a given app's cache.
#'
#' @param appname Character; the name of the application that will "own" the
#'   cache, such as the name of a package.
#'
#' @return The full path to the app's cache directory.
#' @export
#'
#' @examples
#' app_cache_dir("myApp")
app_cache_dir <- function(appname) {
  cache_env <- Sys.getenv(paste0(toupper(appname), "_CACHE_DIR"))
  if (cache_env == "") cache_env <- NULL

  cache_dir <- getOption(paste0(appname, ".dir")) %||%
    cache_env %||%
    rappdirs::user_cache_dir(appname = appname)

  return(
    normalizePath(cache_dir, mustWork = FALSE)
  )
}

#' Set a Cache Directory for an App
#'
#' Override the default paths used by \code{\link{app_cache_dir}}. It is also
#' advised to include this function in \code{.onLoad} for any package that uses
#' a cache.
#'
#' @inheritParams app_cache_dir
#' @param cache_dir Character scalar; a path to a cache directory.
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist. An option is
#'   also set so future calls to \code{\link{app_cache_dir}} will respect the
#'   change.
#' @export
#'
#' @examples
#' # Executing this function creates a cache directory.
#' \donttest{
#' set_app_cache_dir(appname = "dlr", cache_dir = "/my/cache/path")
#' }
set_app_cache_dir <- function(appname, cache_dir = NULL) {
  # Testing this creates directories on the user's system, so I am testing this
  # manually for now.

  # nocov start
  cache_dir <- cache_dir %||%
    app_cache_dir(appname = appname)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  } else {
    # file.access returns 0 for success because it hates clean code. The user
    # has to have read permissions for this function.
    if (file.access(cache_dir, 4) != 0) {
      rlang::abort(
        message = paste("You do not have read access to", cache_dir),
        class = "dir_read_error"
      )
    }
  }

  options(rlang::set_names(list(cache_dir), paste0(appname, ".dir")))

  return(cache_dir)
  # nocov end
}
