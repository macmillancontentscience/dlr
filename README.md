
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dlr

<!-- badges: start -->
<!-- badges: end -->

The goal of dlr is to provide a friendly wrapper around the common
pattern of downloading a file if that file does not already exist
locally.

## Installation

You can install the released version of dlr from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Not yet.
# install.packages("dlr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macmillancontentscience/dlr")
```

## Example

This package is intended primarily to be used in other packages. For
example, you might have a function in your package like this.

``` r
get_foo <- function() {
  # Use dlr to find the cache path. If the file isn't already downloaded, dlr
  # will download it.
  cached_file <- dlr::download_cache(
    url = "https://fake.fake/foo.csv",
    appname = "myCoolPackage"
  )
  
  # Then do the thing you want to do with that cached file.
  return(
    read.csv(cached_file)
  )
}
```

## Code of Conduct

Please note that the dlr project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Disclaimer

This is not an officially supported Macmillan Learning product.

## Contact information

Questions or comments should be directed to Jonathan Bratt
(<jonathan.bratt@macmillan.com>) and Jon Harmon
(<jonthegeek@gmail.com>).
