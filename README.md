
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varutils

<!-- badges: start -->

[![R-CMD-check](https://github.com/ricardo-semiao/varutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardo-semiao/varutils/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The varutils R package is being developed to provide general utility
functions for projects involving Vector Auto-Regressive models, with
specifically the [vars
package](https://cran.r-project.org/web/packages/vars/index.html).

Mainly, the functions retrieve results from *vars* objects, manipulate
them with [tidyverse](https://www.tidyverse.org/), and plot commonly
needed ggplot graphs, in the realm of IRF’s, FEVD’s, predict graphs, VAR
series and residual analysis.

In the future, I intent to add functions for quick (not thorough)
analysis and retrieval of trends, seasonality, and structural breaks,
using common methods and statistical tests. Additionally, I want to
expand the package to other VAR-like objects.

**Disclaimer:** this package is in the early most stage of life. It
hasn’t been thoroughly tested and can present several bugs. I don’t
recommend using it for large-scale projects, yet.

Please report any problems in my email (below) or as a github issue.
Thank you!

Author: Ricardo Semião e Castro (ricardo.semiao@outlook).

## Installation

You can install the development version of varutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ricardo-semiao/varutils")
```

## Upcoming changes

- Rethink how to pass the large number of arguments to ggplot.
- More thorough tests:
  - What errors to expect in `expect_error` calls.
  - Directly test for elements of the ggplots returned by functions.
  - More tests specifically for `test_fun()`.
  - Add tests for “helper” and “setup” functions.
- Rethink the `test` functions list scheme and start using rlang errors.
- Add more info/sections to functions documentation.
- See about solving some less important
  [lintr](https://lintr.r-lib.org/) notes.

## Aditional informations

This package:

- Follows the [tydiverse style guide](https://style.tidyverse.org/).
  - Using the [styler](https://styler.r-lib.org/) and
    [lintr](https://lintr.r-lib.org/) packages for style consistency.
- Uses [testthat](https://testthat.r-lib.org/) and
  [vdiffr](https://vdiffr.r-lib.org/) for automate tests.
- Uses [rlang](https://rlang.r-lib.org/) frameworks for *tidy eval* and
  *rlang errors*.
