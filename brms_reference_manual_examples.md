brms reference manual examples
================
A Solomon Kurz
2019-01-07

These are taken from the `mixture` section of the [brms reference manual](https://cran.r-project.org/web/packages/brms/brms.pdf).

We need data
------------

Here we simulate our data, `dat`.

``` r
library(tidyverse)

set.seed(1234)
dat <- 
  tibble(y = c(rnorm(200, mean = 0, sd = 1), 
               rnorm(100, mean = 6, sd = 1)),
         x = rnorm(300, mean = 0, sd = 1),
         z = sample(0:1, 300, replace = T))

head(dat)
```

    ## # A tibble: 6 x 3
    ##        y       x     z
    ##    <dbl>   <dbl> <int>
    ## 1 -1.21  -0.580      1
    ## 2  0.277 -0.953      0
    ## 3  1.08  -0.179      1
    ## 4 -2.35   1.01       0
    ## 5  0.429  0.0236     0
    ## 6  0.506 -0.649      1

Here's what the data look like.

``` r
library(GGally)
theme_set(theme_grey() +
            theme(panel.grid = element_blank()))

dat %>% 
  mutate(z = factor(z)) %>% 
  
  ggpairs()
```

![](brms_reference_manual_examples_files/figure-markdown_github/unnamed-chunk-2-1.png)

Session info
------------

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] bindrcpp_0.2.2  GGally_1.4.0    forcats_0.3.0   stringr_1.3.1  
    ##  [5] dplyr_0.7.6     purrr_0.2.5     readr_1.1.1     tidyr_0.8.1    
    ##  [9] tibble_1.4.2    ggplot2_3.1.0   tidyverse_1.2.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.4   reshape2_1.4.3     haven_1.1.2       
    ##  [4] lattice_0.20-35    colorspace_1.3-2   htmltools_0.3.6   
    ##  [7] yaml_2.1.19        utf8_1.1.4         rlang_0.3.0.1     
    ## [10] pillar_1.2.3       foreign_0.8-70     glue_1.3.0        
    ## [13] withr_2.1.2        RColorBrewer_1.1-2 modelr_0.1.2      
    ## [16] readxl_1.1.0       bindr_0.1.1        plyr_1.8.4        
    ## [19] munsell_0.5.0      gtable_0.2.0       cellranger_1.1.0  
    ## [22] rvest_0.3.2        psych_1.8.4        evaluate_0.10.1   
    ## [25] labeling_0.3       knitr_1.20         parallel_3.5.1    
    ## [28] broom_0.4.5        Rcpp_1.0.0         scales_1.0.0      
    ## [31] backports_1.1.2    jsonlite_1.5       mnormt_1.5-5      
    ## [34] hms_0.4.2          digest_0.6.18      stringi_1.2.3     
    ## [37] grid_3.5.1         rprojroot_1.3-2    cli_1.0.1         
    ## [40] tools_3.5.1        magrittr_1.5       lazyeval_0.2.1    
    ## [43] crayon_1.3.4       pkgconfig_2.0.1    xml2_1.2.0        
    ## [46] lubridate_1.7.4    reshape_0.8.7      assertthat_0.2.0  
    ## [49] rmarkdown_1.10     httr_1.3.1         rstudioapi_0.7    
    ## [52] R6_2.3.0           nlme_3.1-137       compiler_3.5.1
