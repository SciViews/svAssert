
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ‘SciViews::R’ - Assertions and Meaningful Error Messages <a href="https://www.sciviews.org/svAssert"><img src="man/figures/logo.png" align="right" height="138"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/svAssert/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/svAssert/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/SciViews/svAssert/graph/badge.svg?token=6FYPRdhoFJ)](https://codecov.io/gh/SciViews/svAssert)
[![CRAN
status](https://www.r-pkg.org/badges/version/svAssert)](https://cran.r-project.org/package=svAssert)
[![r-universe
status](https://sciviews.r-universe.dev/badges/svAssert)](https://sciviews.r-universe.dev/svAssert)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The {svAssert} package provides tools for defensive programming in R,
with fast, but versatile assertions partly based on {checkmate}, and
meaningful and rich-formatted error messages using `rlang::abort()` and
`cli::cli_abort()`, rebadged as `stop()` and using the base R mechanism
for message translation in various natural languages.

## Installation

{svAssert} is not available from CRAN yet. You should install it from
the [SciViews R-Universe](https://sciviews.r-universe.dev). To install
this package and its dependencies, run the following command in R:

``` r
install.packages('svAssert', repos = c('https://sciviews.r-universe.dev',
  'https://cloud.r-project.org'))
```

You can also install the latest development version of {svAssert} from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("SciViews/svAssert")
```

R should install all required dependencies automatically, and then it
should compile and install {svAssert}.

## Further explore {svAssert}

You can get further help about this package this way: Make the
{svAssert} package available in your R session:

``` r
library("svAssert")
```

Get help about this package:

``` r
library(help = "svAssert")
help("svAssert-package")
vignette("svAssert")
```

For further instructions, please, refer to the help pages at
<https://www.sciviews.org/svAssert/>.

## Code of Conduct

Please note that the {svAssert} package is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
