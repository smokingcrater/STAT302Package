
# STAT302Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/smokingcrater/STAT302Package/workflows/R-CMD-check/badge.svg)](https://github.com/smokingcrater/STAT302Package/actions)
[![codecov](https://codecov.io/gh/smokingcrater/STAT302Package/branch/master/graph/badge.svg?token=4JQKW7IZPA)](https://codecov.io/gh/smokingcrater/STAT302Package)
<!-- badges: end -->

The goal of STAT302Package is to demonstrate how to build a package.

## Installation

You can install the released version of STAT302Package from GitHub using: 

``` r
devtools::install_github("smokingcrater/STAT302Package")
```

To view vignettes, run the following code:

``` r
devtools::install_github("smokingcrater/STAT302Package", build_vignette = TRUE, build_opts = c())
library(STAT302Package)
# Use this to view the vignette in the STAT302Package HTML help
help(package = "STAT302Package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302Package")
```
