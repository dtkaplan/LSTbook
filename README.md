
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LST: *Lessons in Statistical Thinking*

<!-- badges: start -->
<!-- badges: end -->

The `{LST}` package provides software and datasets for *Lessons in
Statistical Thinking*.

## Installation

You can install the development version of LST from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dtkaplan/LST")
```

## Example

``` r
library(LST)
#> Loading required package: mosaic
#> Registered S3 method overwritten by 'mosaic':
#>   method                           from   
#>   fortify.SpatialPolygonsDataFrame ggplot2
#> 
#> The 'mosaic' package masks several functions from core packages in order to add 
#> additional features.  The original behavior of these functions should not be affected by this.
#> 
#> Attaching package: 'mosaic'
#> The following objects are masked from 'package:dplyr':
#> 
#>     count, do, tally
#> The following object is masked from 'package:Matrix':
#> 
#>     mean
#> The following object is masked from 'package:ggplot2':
#> 
#>     stat
#> The following objects are masked from 'package:stats':
#> 
#>     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
#>     quantile, sd, t.test, var
#> The following objects are masked from 'package:base':
#> 
#>     max, mean, min, prod, range, sample, sum
#> Loading required package: ciTools
#> ciTools version 0.6.1 (C) Institute for Defense Analyses
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
