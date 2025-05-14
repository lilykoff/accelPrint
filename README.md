
<!-- README.md is generated from README.Rmd. Please edit that file -->

# accelPrint

<!-- badges: start -->

[![R-CMD-check](https://github.com/lilykoff/accelPrint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lilykoff/accelPrint/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of accelPrint is to do fingerprinting for acceleration and
walking.

## Installation

You can install the development version of accelPrint from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lilykoff/accelPrint")
```

## Example

This is a basic example which shows you how to use the package

### Get walking bouts from raw accelerometry data

We compute the bouts and plot the signal from the first 10s. There is no
data plotted in second 5 because no steps were identified by ADEPT in
that second.

``` r
library(accelPrint)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
# load example data 
data(raw_accel)
# run the get walking function
walking_bouts = accelPrint::get_walking(raw_accel, parallel = TRUE)
#> Sample rate not provided. Inferred sample rate: 100 Hz
#> Parallel processing is enabled, but cores are not specified. Using all available cores (8)
#> ADEPT completed
head(walking_bouts) 
#> # A tibble: 6 × 6
#>   time                     x      y      z second              bout_seconds
#>   <dttm>               <dbl>  <dbl>  <dbl> <dttm>                     <int>
#> 1 2000-01-01 12:04:02 -0.711 -0.555  0.016 2000-01-01 12:04:02           20
#> 2 2000-01-01 12:04:02 -0.387 -0.387  0.152 2000-01-01 12:04:02           20
#> 3 2000-01-01 12:04:02 -0.141 -0.258  0.207 2000-01-01 12:04:02           20
#> 4 2000-01-01 12:04:02 -0.145 -0.148  0.168 2000-01-01 12:04:02           20
#> 5 2000-01-01 12:04:02 -0.379 -0.012  0.051 2000-01-01 12:04:02           20
#> 6 2000-01-01 12:04:02 -0.594  0.121 -0.09  2000-01-01 12:04:02           20

walking_bouts %>% 
  slice(1:(10 * 100)) %>% # take first 10 s 
  mutate(vm = sqrt(x^2 + y^2 + z^2)) %>% # compute vector magnitude
  ggplot(aes(x = time, y = vm, group=second)) + 
  geom_line() + 
  scale_x_datetime(date_labels = "%S") + 
  labs(x = "Time (s)", y = "Vector magnitude (g)")
```

<img src="man/figures/README-example-1.png" width="100%" />

### Get fingerprint predictors from walking bouts

Each column is one grid cell/lag combination, each row is a second. Each
entry is the number of points in the grid cell for that lag. Column
names are formatted as `range_lagrange_lag`, so `[0,0.25]_[0,0.25]_15`
corresponds to number of points in range acceleration $\in [0,0.25]$,
lag acceleration $\in [0,0.25]$, for a lag of 15 samples.

``` r
# run the get grid cells function on the walking bouts
# specify lags of 0.15, 0.30, 0.45 seconds and grid cell size of 0.25g 
fingerprint_predictors = compute_grid_cells(walking_bouts, 
                                            lags = c(0.15, 0.30, 0.45), 
                                            cell_size = 0.25,
                                            max_vm = 3)
#> Sample rate not provided. Inferred sample rate: 100 Hz

head(fingerprint_predictors) 
#> # A tibble: 6 × 433
#>   second              `[0,0.25]_[0,0.25]_15` `[0,0.25]_(0.25,0.5]_15`
#>   <dttm>                               <int>                    <int>
#> 1 2000-01-01 12:04:02                      0                        0
#> 2 2000-01-01 12:04:03                      0                        0
#> 3 2000-01-01 12:04:04                      0                        0
#> 4 2000-01-01 12:04:06                      0                        0
#> 5 2000-01-01 12:04:07                      0                        0
#> 6 2000-01-01 12:04:08                      0                        0
#> # ℹ 430 more variables: `[0,0.25]_(0.5,0.75]_15` <int>,
#> #   `[0,0.25]_(0.75,1]_15` <int>, `[0,0.25]_(1,1.25]_15` <int>,
#> #   `[0,0.25]_(1.25,1.5]_15` <int>, `[0,0.25]_(1.5,1.75]_15` <int>,
#> #   `[0,0.25]_(1.75,2]_15` <int>, `[0,0.25]_(2,2.25]_15` <int>,
#> #   `[0,0.25]_(2.25,2.5]_15` <int>, `[0,0.25]_(2.5,2.75]_15` <int>,
#> #   `[0,0.25]_(2.75,3]_15` <int>, `(0.25,0.5]_[0,0.25]_15` <int>,
#> #   `(0.25,0.5]_(0.25,0.5]_15` <int>, `(0.25,0.5]_(0.5,0.75]_15` <int>, …
```

``` r
# plot 
plot_grid_cells(fingerprint_predictors)
```
