
# USK

<!-- badges: start -->
<!-- badges: end -->

*usktest* is a function used to do the Scott-Knott cluster analysis
(1974) for unbalanced designs proposed at 2017 (CONRADO, T. V; FERREIRA,
D. F.; SCAPIM, C. A.; MALUF, W. R.). To learn more, see the article:
<http://ref.scielo.org/ws792m>.

## Installation

You can install the development version from
[github](https://github.com/felipe179971/USK) with:

``` r
# install.packages("devtools")
devtools::install_github("felipe179971/USK",force = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(USK)
## basic example code
set.seed(3)
taus=c(4,4,-4,-4,9,-9)
treatment<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
error<-rnorm(3*length(taus),0,1)
y<-2+taus+error
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dataset<-data.frame(y,treatment)

test<-usktest(y~treatment,dataset)
#> [1] "##########################ANOVA###########################"
#>             Df Sum Sq Mean Sq F value   Pr(>F)    
#> treatment    5  671.1  134.22   201.6 1.98e-10 ***
#> Residuals   11    7.3    0.67                     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 1 observation deleted due to missingness
#> [1] "#######################Scott-Knott########################"
#> # A tibble: 6 x 5
#> # Groups:   treatment [6]
#>   treatment Group  Mean   min    max
#>   <fct>     <fct> <dbl> <dbl>  <dbl>
#> 1 trat 5    a     10.5  10.0  11.2  
#> 2 trat 2    b      6.36  5.71  7.12 
#> 3 trat 1    b      5.47  5.04  6.09 
#> 4 trat 4    c     -1.94 -3.15 -0.733
#> 5 trat 3    c     -2.27 -3.22 -1.74 
#> 6 trat 6    d     -7.58 -8.13 -6.97
##ggplot2
plot_usk(test)
```

<img src="man/figures/README-example-1.png" width="90%" />

``` r
##plotly
#Run
#plotly_usk(test)
```
