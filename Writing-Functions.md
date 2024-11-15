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

    ##  [1]  0.72175164  0.91367858 -1.55067241 -1.15372975 -0.09319324  0.31585677
    ##  [7] -0.69178624 -1.30299720 -1.11094529  1.25843109  0.90543755  0.53809002
    ## [13]  0.97183463 -0.28239639 -0.71642668  1.45603640 -0.50880371 -1.42205306
    ## [19] -0.05745445 -0.02096131  0.96059387  0.53532247  0.60873434 -2.01559504
    ## [25]  1.38183675 -0.77009307 -0.04724863 -0.90413636  0.48168029  1.59920841

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

    ##  [1]  0.72175164  0.91367858 -1.55067241 -1.15372975 -0.09319324  0.31585677
    ##  [7] -0.69178624 -1.30299720 -1.11094529  1.25843109  0.90543755  0.53809002
    ## [13]  0.97183463 -0.28239639 -0.71642668  1.45603640 -0.50880371 -1.42205306
    ## [19] -0.05745445 -0.02096131  0.96059387  0.53532247  0.60873434 -2.01559504
    ## [25]  1.38183675 -0.77009307 -0.04724863 -0.90413636  0.48168029  1.59920841

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

## Multiple outputs

``` r
mean_and_sd = function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Check that the function works.

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.03  3.20

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data |>
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.53  2.84

``` r
sim_mean_sd = function(samp_size, mu, sigma) {
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )
  
  sim_data |>
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.03  3.27
