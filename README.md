
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

# Get asymptotic mass values for a list of species (column in tibble)
example_data |> 
  lookup_lmax(full_load = F)
#> # A tibble: 20 x 4
#>   Species              abundance site   Lmax
#>   <chr>                    <int> <chr> <dbl>
#> 1 Margarites costalis          6 A      5.89
#> 2 Hemimyzon khonensis          3 A      6.58
#> 3 Tytthocope pygmaea           5 A      8.47
#> 4 Pempheris schreineri         8 A     14.7 
#> 5 Cubaris invenustus           4 A      8.47
#> # ... with 15 more rows


# Get length-weight parameter (a, b) estimates for a list of species
example_data |> 
  lookup_lw(full_load = F)
#> # A tibble: 20 x 5
#>   Species              abundance site       a     b
#>   <chr>                    <int> <chr>  <dbl> <dbl>
#> 1 Margarites costalis          6 A     1.28    2.71
#> 2 Hemimyzon khonensis          3 A     0.0158  3.04
#> 3 Tytthocope pygmaea           5 A     0.0244  2.46
#> 4 Pempheris schreineri         8 A     0.0180  3.01
#> 5 Cubaris invenustus           4 A     0.0244  2.46
#> # ... with 15 more rows


example_data |> 
  lookup_lmax(full_load = F) |> 
  lookup_lw(full_load = F) |> 
  estimate_SSD_params()
#> # A tibble: 20 x 9
#>   Species              abundance site   Lmax      a     b   Mmax meanlog sdlog
#>   <chr>                    <int> <chr> <dbl>  <dbl> <dbl>  <dbl>   <dbl> <dbl>
#> 1 Margarites costalis          6 A      5.89 1.28    2.71 158.     2.94  0.995
#> 2 Hemimyzon khonensis          3 A      6.58 0.0158  3.04   4.83   0.219 0.995
#> 3 Tytthocope pygmaea           5 A      8.47 0.0244  2.46   4.68   0.195 0.995
#> 4 Pempheris schreineri         8 A     14.7  0.0180  3.01  59.3    2.18  0.995
#> 5 Cubaris invenustus           4 A      8.47 0.0244  2.46   4.68   0.195 0.995
#> # ... with 15 more rows


example_data |> 
  lookup_lmax(full_load = F) |> 
  lookup_lw(full_load = F) |> 
  estimate_SSD_params() |> 
  calc_CSS(group_var = "site")
#> # A tibble: 10 x 4
#> # Groups:   site [1]
#>   site      m norm_density density
#>   <chr> <dbl>        <dbl>   <dbl>
#> 1 A         3        5.72    11.4 
#> 2 A         6        2.74    10.9 
#> 3 A        12        1.27    10.1 
#> 4 A        24        0.505    8.07
#> 5 A        48        0.162    5.20
#> # ... with 5 more rows


example_data |> 
  lookup_lmax(full_load = F) |> 
  lookup_lw(full_load = F) |> 
  estimate_SSD_params() |> 
  calc_CSS(group_var = "site") |> 
  plot_CSS()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r

example_data |> 
  lookup_lmax(full_load = F) |> 
  lookup_lw(full_load = F) |> 
  estimate_SSD_params() |> 
  plot_SSDs(legend = F) 
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/FreddieJH/sbss/issues).

------------------------------------------------------------------------
