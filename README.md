
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sbss <img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
<!-- badges: end -->

## Overview

**S**pecies-**B**ased **S**ize **S**pectra (sbss) allows for the
construction of community-level empirical size spectra based on
species-level information, including in data-poor situations.

## Installation

``` r
# install.packages("devtools")
library(devtools)

devtools::install_github("freddiejh/sbss")
library(sbss)
```

## Usage

``` r
library(sbss)
library(dplyr)

# my_spp <-
#   load_taxalist() %>% 
#   head(6) %>%
#   dplyr::select(Species)
# 
# lookup_lmax(my_spp)
# lookup_lw(my_spp)
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/FreddieJH/sbss/issues).

------------------------------------------------------------------------
