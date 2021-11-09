iteration
================
Anyu Zhu
11/4/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.83806089  1.18699775  0.75368698 -0.75201537  0.13676132  0.11174155
    ##  [7] -0.94666490 -0.31184629  0.58310067  0.43274914 -1.19721543  1.13933827
    ## [13]  1.11433475  0.48764697 -1.66760368  1.33056742 -1.59629115 -0.51161702
    ## [19]  0.25597660 -1.05250992 -0.01320674  0.33011966  0.09970308 -1.90743945
    ## [25]  0.15562491

``` r
z_scores = function(x){
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  1.83806089  1.18699775  0.75368698 -0.75201537  0.13676132  0.11174155
    ##  [7] -0.94666490 -0.31184629  0.58310067  0.43274914 -1.19721543  1.13933827
    ## [13]  1.11433475  0.48764697 -1.66760368  1.33056742 -1.59629115 -0.51161702
    ## [19]  0.25597660 -1.05250992 -0.01320674  0.33011966  0.09970308 -1.90743945
    ## [25]  0.15562491

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)
z_scores(y_vec)
```

    ##  [1] -0.17605715  0.59947597  0.22798058  0.63172089 -0.42459148 -2.50557138
    ##  [7]  0.63377706  0.13709531 -1.07131016 -0.42686034  0.53548149 -0.28241152
    ## [13]  0.45771784 -0.79375405  1.23646426 -0.98545807 -2.52916312  0.32629073
    ## [19] -1.17596606 -0.80516837  2.45675710  0.60283773 -0.04815152  1.05336822
    ## [25]  1.68397373  0.50263325 -0.40066590  0.57549035  0.62273508 -1.16438058
    ## [31] -0.58114495  0.20213980 -1.27225900  0.12822715  0.91182078  0.51656098
    ## [37]  0.49600427 -0.54842575  1.03463993 -0.38185309

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
    ## 1  5.75  4.07

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.323

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
    ## 1  2.11  2.82

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
    ## 1  4.14  2.90

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
    ##  1 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  2 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  3 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  4 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  5 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  7 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  8 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  9 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ## 10 Painful                                               1.0 ou… "\n  I think I…
    ## # … with 40 more rows

## Lists

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, FALSE),
  summary = summary(rnorm(1000, mean = 5, sd = 3))
)

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.868   2.759   4.785   4.951   7.014  16.606

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.868   2.759   4.785   4.951   7.014  16.606

## List of normals

``` r
list_norms = 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 10, sd = 1.5),
    d = rnorm(50, mean = -8, sd = 4)
  )

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.02 0.916

## for loop

to iterate over the list of normals

``` r
output = vector("list", length = 4)

for (i in 1:4){
  output[[1]] = mean_and_sd(list_norms[[1]])
}
```

use map:

``` r
output = map(list_norms, mean_and_sd)

output_2 = map(list_norms, summary)
```

## List Columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )

listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1] 3.2027129 2.4688444 3.8865517 1.7503735 4.0428166 2.5429525 3.6707288
    ##  [8] 0.6543029 0.2247970 2.7941764 3.1507273 2.1481359 2.2300021 2.0986254
    ## [15] 0.4125481 2.2877658 3.0696734 2.4124142 2.3671575 1.0015335 0.7880474
    ## [22] 1.4088499 2.1489695 1.4740459 0.6427072 2.0812646 1.9070211 3.1859045
    ## [29] 1.2900811 1.0556564 2.4271629 2.5947814 1.5554328 1.3522913 1.9391239
    ## [36] 1.6919265 3.1469882 1.6933048 1.9547846 0.9409336 0.3170071 3.0347665
    ## [43] 2.0400039 1.5990967 2.7419118 2.5358967 1.6704773 1.0728013 1.7775722
    ## [50] 2.2747954
    ## 
    ## $b
    ##  [1]  6.5125876  9.0496544 -0.5341515 10.8721421  3.0817118  1.4277786
    ##  [7]  3.1339644  4.9163068  7.0790417  4.5121971  6.8249295  3.5060591
    ## [13]  3.5408516 10.0053602 11.9290352  3.9460745  5.2122797  6.4916579
    ## [19] 10.8383464 -1.8973408  4.0141571 10.7161305  2.6775156  5.2494982
    ## [25]  5.7847782  1.2201772  5.6255984  2.7285308  9.2782326  7.2741159
    ## [31]  5.5526620  7.8014567  5.9978467  4.9363828  1.0774514  3.6802936
    ## [37]  4.4932009  5.9553977  8.4121004  3.6007241 10.0663061  5.0490723
    ## [43]  1.6685406  5.4230376  1.1096855 -1.0043275  2.5840555  9.6970591
    ## [49]  8.4774352  4.2802426
    ## 
    ## $c
    ##  [1]  7.432201  9.866862  8.836759 10.460136  8.849020  9.317890  7.990091
    ##  [8] 11.023860 11.044523 11.046950 10.427261 12.459348 10.276681 11.366012
    ## [15]  9.886693  9.953207  9.675148  9.942665 10.071842  9.035110  7.998070
    ## [22] 12.407472  9.677566 10.969393 11.518320 11.233344  9.248236  8.522504
    ## [29] 10.051782 12.175112  8.695625  8.656332 10.422294 11.824411  7.446714
    ## [36] 10.232483  9.594442 11.563920 10.055355  8.962774  9.116491 10.378733
    ## [43] 10.747379  9.734099  8.255074 11.662988 11.190972 10.928029  8.814392
    ## [50]  9.217702
    ## 
    ## $d
    ##  [1]  -9.1931621  -9.1680543 -10.2448821 -10.5786256  -6.7316258  -9.2460501
    ##  [7]  -9.4535153  -4.5952659 -11.9869559  -6.0786807  -5.5779409 -11.6861815
    ## [13]  -7.8886946  -9.4176964  -6.2844644 -10.4782901  -6.2109417  -5.5146586
    ## [19]  -9.1667124  -4.6939902  -7.1359278  -7.5934733  -4.1307633 -11.6531054
    ## [25]  -7.3959799 -12.8420059  -1.9247155  -8.3768962 -13.5173977  -8.9334555
    ## [31]  -6.9934138  -9.0942255 -16.1474538  -8.4595910  -7.8792083  -5.2367282
    ## [37]  -7.6006649 -10.6237104   0.4432377  -3.9217133  -6.7276385  -5.0630203
    ## [43]  -2.9884634  -6.7248774  -6.1042069 -10.4104575  -5.4999498  -4.1190856
    ## [49]  -5.4299740  -5.3025154

``` r
listcol_df %>% filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% 
  mutate(summaries = map(listcol_df$norms, mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:29:26 (7.602)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:29:30 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:29:32 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
weather_nested = nest(weather_df, data = date:tmin)

unnest(weather_nested, data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
weather_nested %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
lm(tmax ~ tmin, data = weather_nested$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nested$data[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_lm = function(df){
  lm(tmax ~ tmin, data = df)
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

## Napoleon

``` r
map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  2 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  3 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  4 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  5 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  7 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  8 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  9 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ## 10 Painful                                               1.0 ou… "\n  I think I…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  2 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie…
    ##  3 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my…
    ##  4 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio…
    ##  5 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ##  6 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i…
    ##  7 Your mom went to college.         5.0 out of 5 stars "\n  Classic funny movi…
    ##  8 Very funny movie                  5.0 out of 5 stars "\n  I watch this movie…
    ##  9 Watch it twice! Trust me!         5.0 out of 5 stars "\n  Nothing to dislike…
    ## 10 A classic                         5.0 out of 5 stars "\n  If you don’t enjoy…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  2 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  3 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  4 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  5 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ##  6 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ##  7 Love it                                     5.0 out of 5 stars "\n  What of …
    ##  8 WORTH IT!                                   5.0 out of 5 stars "\n  It's the…
    ##  9 Funny movie.                                5.0 out of 5 stars "\n  Great co…
    ## 10 Best movie ever!                            5.0 out of 5 stars "\n  Got this…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  2 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  3 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  4 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ##  5 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ##  6 Perfect                                       5.0 out of 5 stars "\n  Exactl…
    ##  7 Love this movie!                              5.0 out of 5 stars "\n  Great …
    ##  8 Love it                                       5.0 out of 5 stars "\n  Love t…
    ##  9 As described                                  3.0 out of 5 stars "\n  Book i…
    ## 10 GOSH!!!                                       5.0 out of 5 stars "\n  Just w…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 Watch it right now                5.0 out of 5 stars "\n  You need to watch …
    ##  2 At this point it’s an addiction   5.0 out of 5 stars "\n  I watch this movie…
    ##  3 💕                                5.0 out of 5 stars "\n  Hands down, one of…
    ##  4 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  5 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ##  6 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  7 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik…
    ##  8 So Funny                          5.0 out of 5 stars "\n  This is such a goo…
    ##  9 Best movie ever                   5.0 out of 5 stars "\n  It's napoleon dyna…
    ## 10 Funny                             5.0 out of 5 stars "\n  Classic\n"

``` r
napoleon_df = tibble(
  urls = urls
)

napoleon_df %>% 
  mutate(reviews = map(urls, get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  2 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  3 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  4 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  5 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  7 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  8 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  9 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ## 10 Painful                                               1.0 ou… "\n  I think I…
    ## # … with 40 more rows
