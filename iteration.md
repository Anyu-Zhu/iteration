iteration
================
Anyu Zhu
11/4/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.04179257 -1.16689401  0.72689589 -0.01108940  0.09877958 -2.63147544
    ##  [7]  0.82819837  1.33374906  0.05484222  0.35311853  0.73985011  0.25110187
    ## [13]  0.23399614  1.22196622  0.25348449 -2.24184145 -1.11863069  0.66564354
    ## [19] -0.53431083 -0.39099235 -0.35820658  0.87366379 -0.26362937  1.44141418
    ## [25] -0.40142645

``` r
z_scores = function(x){
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.04179257 -1.16689401  0.72689589 -0.01108940  0.09877958 -2.63147544
    ##  [7]  0.82819837  1.33374906  0.05484222  0.35311853  0.73985011  0.25110187
    ## [13]  0.23399614  1.22196622  0.25348449 -2.24184145 -1.11863069  0.66564354
    ## [19] -0.53431083 -0.39099235 -0.35820658  0.87366379 -0.26362937  1.44141418
    ## [25] -0.40142645

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)
z_scores(y_vec)
```

    ##  [1]  2.21127772  1.40889465 -1.86122873  0.48737884  0.10217487  1.58433781
    ##  [7]  1.26459963 -0.02914884  0.12610911  0.49078950 -1.13851570  0.06669320
    ## [13] -0.98321085 -0.17434076  1.88459617 -1.47112398 -0.49769541 -0.14007082
    ## [19] -1.11964466  0.48505117  0.35395186 -0.32232934  0.57947039  0.13372854
    ## [25] -0.72118575 -0.79878606 -1.69032134  1.00963494  1.75664852 -0.85202644
    ## [31]  0.52980333 -1.15932310 -0.21591672  0.54338765  0.32713513 -1.15696603
    ## [37] -1.06537813 -0.15537223  0.45995215 -0.25303028

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
    ## 1  6.42  4.73

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.328

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
    ## 1  2.37  2.77

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
    ## 1  3.39  2.54

## Napoleon dynamics

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Write a function gets reviews based on page url

``` r
get_page_reviews = function(page_url){
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows
