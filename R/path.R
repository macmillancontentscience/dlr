#' Path to an App Cache Directory
#'
#' App cache directories can depend on the user's operating system and an
#' overall \code{R_USER_CACHE_DIR} environment variable. This function
#'
#' @param appname Character; the name of the application that will "own" this
#'   file, such as the name of a package.
#'
#' @return The full path to the app's cache directory.
#' @export
#'
#' @examples
#' app_cache_dir("myApp")
app_cache_dir <- function(appname) {
  # This is a simple wrapper around rappdirs::user_cache_dir. I'm pulling it
  # into its own function because I plan to add ways to customize the path
  # per-app.
  return(
    rappdirs::user_cache_dir(appname = appname)
  )
}
