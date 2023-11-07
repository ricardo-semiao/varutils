
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varutils

<!-- badges: start -->

[![R-CMD-check](https://github.com/ricardo-semiao/varutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardo-semiao/varutils/actions/workflows/R-CMD-check.yaml)
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

**Disclaimer:** this package is in costant development, and in the most
early stages of life. It hasn’t been thorougly tested and can present
several bugs. Please report any problems in my email (below) or as a
github issue. Thank you for the attention.

Author: Ricardo Semião e Castro (ricardo.semiao@outlook)

## Installation

You can install the development version of varutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ricardo-semiao/varutils")
```
