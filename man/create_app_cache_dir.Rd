% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{create_app_cache_dir}
\alias{create_app_cache_dir}
\title{Create a Cache Directory for an App}
\usage{
create_app_cache_dir(appname)
}
\arguments{
\item{appname}{Character; the name of the application that will "own" the
cache, such as the name of a package.}
}
\value{
A normalized path to a cache directory. The directory is created if
the user has write access and the directory does not exist.
}
\description{
Create the default path expected by \code{\link[=app_cache_dir]{app_cache_dir()}}.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Executing this function creates a cache directory.
create_app_cache_dir("dlr")
\dontshow{\}) # examplesIf}
}
