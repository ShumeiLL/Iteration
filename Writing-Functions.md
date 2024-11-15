Writing Functions
================
Shumei Liu
2024-11-14

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec) #z-score
```

    ##  [1]  0.60320419 -1.09237749 -0.38907126  0.01425970  0.26148402 -0.84344957
    ##  [7]  0.78564032  0.26627258  0.52485290 -0.92539681  2.64240932  1.25191750
    ## [13] -1.66299455  0.74528214  1.66870916  0.06471544 -1.12901188 -0.59201193
    ## [19]  0.06542826 -1.38092997 -0.88972166 -0.60155032 -0.18060452  0.04961636
    ## [25]  0.76518911  1.14299255 -0.55614370 -0.05166190 -1.49263490  0.93558690

I want a function to compute z-scores

``` r
z_scores = function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  z = (x-mean(x)) / sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1]  0.60320419 -1.09237749 -0.38907126  0.01425970  0.26148402 -0.84344957
    ##  [7]  0.78564032  0.26627258  0.52485290 -0.92539681  2.64240932  1.25191750
    ## [13] -1.66299455  0.74528214  1.66870916  0.06471544 -1.12901188 -0.59201193
    ## [19]  0.06542826 -1.38092997 -0.88972166 -0.60155032 -0.18060452  0.04961636
    ## [25]  0.76518911  1.14299255 -0.55614370 -0.05166190 -1.49263490  0.93558690

Try my function on some other things. These should give errors.

``` r
z_scores(3) #sd(3) = NA
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("My name is Shumei") #cannot get the mean of character
```

    ## Error in z_scores("My name is Shumei"): Input must be numeric

``` r
z_scores(mtcars) #cannot get the mean of a dataset
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE)) #sequence with 0 and 1
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric
