
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
devtools::install_github("freddiejh/sbss")
```

## Usage

``` r
library(sbss)

# Get asymptotic mass values for a list of species
example_species |> 
  lookup_lmax(full_load = F)
#> # A tibble: 20 x 2
#>   Species                   Lmax
#>   <chr>                    <dbl>
#> 1 Acrilloscala lamyi        6.29
#> 2 Acropora samoensis        3   
#> 3 Novactaea bella          13.0 
#> 4 Collisella asmi           8.68
#> 5 Echinorhynchus debenhami 46.2 
#> # ... with 15 more rows


# Get length-weight parameter (a, b) estimates for a list of species
example_species |> 
  lookup_lw(full_load = F)
#> # A tibble: 20 x 3
#>   Species                       a     b
#>   <chr>                     <dbl> <dbl>
#> 1 Acrilloscala lamyi       0.462   2.82
#> 2 Acropora samoensis       0.0059  2.82
#> 3 Novactaea bella          0.495   2.86
#> 4 Collisella asmi          0.204   3.06
#> 5 Echinorhynchus debenhami 0.142   3.00
#> # ... with 15 more rows
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/FreddieJH/sbss/issues).

------------------------------------------------------------------------
