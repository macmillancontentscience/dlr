% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{.is_url}
\alias{.is_url}
\title{Determine if Path is a URL}
\usage{
.is_url(source_path)
}
\arguments{
\item{source_path}{Character; the path to check.}
}
\value{
Logical indicating whether that looks like a url (\code{TRUE} if it starts
with \verb{http://}, \verb{https://}, \verb{ftp://}, or \verb{ftps://}, \code{FALSE} otherwise).
}
\description{
Determine if Path is a URL
}
\keyword{internal}
