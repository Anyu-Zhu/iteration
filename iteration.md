iteration
================
Anyu Zhu
11/4/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.274884461  0.267530092  2.031334191  0.750447158 -0.237896704
    ##  [6]  0.717435897 -0.107630662 -0.154048658 -0.337993708 -1.716277875
    ## [11] -0.697465842 -1.290543220  0.794275517 -0.683787281 -1.004335887
    ## [16] -0.001601974 -0.979721277  0.945575721  2.032982696  0.187669742
    ## [21] -0.823619962  0.205245416  1.679322179  0.048533842 -0.350544940

``` r
z_scores = function(x){
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -1.274884461  0.267530092  2.031334191  0.750447158 -0.237896704
    ##  [6]  0.717435897 -0.107630662 -0.154048658 -0.337993708 -1.716277875
    ## [11] -0.697465842 -1.290543220  0.794275517 -0.683787281 -1.004335887
    ## [16] -0.001601974 -0.979721277  0.945575721  2.032982696  0.187669742
    ## [21] -0.823619962  0.205245416  1.679322179  0.048533842 -0.350544940

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)
z_scores(y_vec)
```

    ##  [1]  0.18806402 -0.40762671 -0.67032780  1.01088567  0.31883614 -0.50062380
    ##  [7]  1.07254246  0.14916856 -1.16925625  0.64815509  1.60050462  0.15798405
    ## [13]  2.08245953 -1.19953966 -0.78900633  0.54861773 -0.17792431  0.96905368
    ## [19] -1.30028397 -2.45192865 -1.16895321  0.03473053 -0.50230206  0.08603133
    ## [25]  0.07404008  0.91658040 -1.36580533  1.11885859  0.29406707 -0.19735850
    ## [31]  0.62570877  1.26004741 -0.17254589 -1.46244924  0.63708620 -0.68213661
    ## [37]  1.33061496 -1.74576047  0.14547755  0.69431436

``` r
z_scores = function(x){
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(c("a", "b", "c", "d"))
```

    ## Error in z_scores(c("a", "b", "c", "d")): x needs to be numeric

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

## Multiple outputs

``` r
mean_and_sd = function(x){
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(mean = mean_x,
           sd = sd_x)
  
  return(output_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.26  3.11

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.314

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.97  2.70

function that simulates data, compute mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma){
  sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.80  3.17
