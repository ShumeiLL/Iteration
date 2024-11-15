Iteration and List Columns
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

## Lists

You can put anything in a list.

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)
```

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.13802 -0.66838  0.03123  0.02351  0.70306  3.37866

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[[1]][1:3]
```

    ## [1] 5 6 7

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norms = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(20, 0, 5),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )

is.list(list_norms)
```

    ## [1] TRUE

Pause and get the old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.92 0.772

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.40  5.43

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.180

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.35  1.09

Let’s use a `for` loop.

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.92 0.772
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.40  5.43
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.180
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.35  1.09

## `map`

``` r
output = map(list_norms, mean_and_sd) #list_norms to keep the name of each distribution

output
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.92 0.772
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.40  5.43
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.180
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.35  1.09

what if you want a different function?

``` r
output = map(list_norms, median)

output
```

    ## $a
    ## [1] 2.720587
    ## 
    ## $b
    ## [1] 1.75749
    ## 
    ## $c
    ## [1] 10.05231
    ## 
    ## $d
    ## [1] -3.088368

## `map` variants变体

``` r
output = map_dbl(list_norms, median, .id = "input")

output
```

    ##         a         b         c         d 
    ##  2.720587  1.757490 10.052313 -3.088368

We use `map_dbl` because median outputs a single numeric value each
time; the result is a vector instead of a list. Using the `.id` argument
keeps the names of the elements in the input list.

If we tried to use `map_int` or `map_lgl`, we’d get an error because the
output of median isn’t a integer or a logical.

``` r
output = map_dfr(list_norms, mean_and_sd, .id = "input") 

output
```

    ## # A tibble: 4 × 3
    ##   input  mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      2.92 0.772
    ## 2 b      1.40 5.43 
    ## 3 c     10.0  0.180
    ## 4 d     -3.35 1.09

``` r
# output = map2(input_1, input_2, \(x,y) func(arg_1 = x, arg_2 = y)), two arguments
```

## List columns and operations

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )
```

``` r
listcol_df |> pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df |> pull(samp)
```

    ## $a
    ##  [1] 2.188089 3.714633 2.606640 2.660567 2.576786 3.269309 4.046535 3.630332
    ##  [9] 2.233399 3.906140 3.506102 2.731946 2.386891 1.388895 3.532738 1.944986
    ## [17] 4.106331 3.224201 2.105474 2.709229
    ## 
    ## $b
    ##  [1]   8.36752356  -4.59469040 -10.14432397   2.77868913  -1.04802905
    ##  [6]   7.10007665  -2.14576483   8.00200540   5.30234384   3.10848691
    ## [11]  -2.18931745   9.42431366   1.05774855   6.00893957   4.58027585
    ## [16]  -8.74176903   2.37940152  -0.05798579  -2.29212162   1.13557933
    ## 
    ## $c
    ##  [1]  9.743637 10.178718  9.664778  9.964040 10.088115 10.302000 10.214919
    ##  [8]  9.892623 10.016510 10.004867 10.174824  9.912785 10.108180  9.894845
    ## [15] 10.213176 10.143534 10.099002  9.732962  9.961835 10.223041
    ## 
    ## $d
    ##  [1] -2.511050 -2.194012 -4.580195 -4.387725 -2.423981 -2.485608 -2.892338
    ##  [8] -5.041127 -2.000454 -2.941001 -4.853533 -1.549040 -3.741921 -2.730437
    ## [15] -3.456919 -3.698355 -2.898455 -5.453841 -3.235735 -4.004699

Let’s try some operations.

``` r
listcol_df$samp[[1]]
```

    ##  [1] 2.188089 3.714633 2.606640 2.660567 2.576786 3.269309 4.046535 3.630332
    ##  [9] 2.233399 3.906140 3.506102 2.731946 2.386891 1.388895 3.532738 1.944986
    ## [17] 4.106331 3.224201 2.105474 2.709229

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.92 0.772

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.92 0.772
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.40  5.43
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.180
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.35  1.09

add a list column

``` r
listcol_df = 
  listcol_df |> 
  mutate(summary = map(samp, mean_and_sd),
         medians = map_dbl(samp, median))
  
listcol_df
```

    ## # A tibble: 4 × 4
    ##   name  samp         summary          medians
    ##   <chr> <named list> <named list>       <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>    2.72
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>    1.76
    ## 3 c     <dbl [20]>   <tibble [1 × 2]>   10.1 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>   -3.09
