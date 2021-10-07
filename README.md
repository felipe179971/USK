Habits
================

# USK

<!-- badges: start -->
<!-- badges: end -->

*usktest* is a function used to do the Scott-Knott cluster analysis
(1974) for unbalanced designs proposed at 2017 (CONRADO, T. V; FERREIRA,
D. F.; SCAPIM, C. A.; MALUF, W. R.). To learn more, see the article:
<http://ref.scielo.org/ws792m>.

## Installation

You can install the the development version from
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
taus=c(4,4,-4,-4,9,-9)
Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
erro<-rnorm(3*length(taus),0,1)
y<-2+taus+erro
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

plot_usk(usktest(y~Tratamento,dados,ANOVA = T))
#> [1] "##########################ANOVA###########################"
#>             Df Sum Sq Mean Sq F value   Pr(>F)    
#> Tratamento   5  698.3  139.65   260.2 4.94e-11 ***
#> Residuals   11    5.9    0.54                     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 1 observation deleted due to missingness
#> [1] "#######################Scott-Knott########################"
#> # A tibble: 6 x 5
#> # Groups:   Tratamento [6]
#>   Tratamento Group  Mean   min   max
#>   <fct>      <fct> <dbl> <dbl> <dbl>
#> 1 trat 5     a     11.1   9.97 12.0 
#> 2 trat 1     b      5.97  5.36  6.44
#> 3 trat 2     b      5.4   4.39  6.42
#> 4 trat 3     c     -2.27 -2.57 -1.98
#> 5 trat 4     c     -2.63 -2.95 -2.02
#> 6 trat 6     d     -7.43 -7.72 -6.98
```

<img src="man/figures/README-example-1.png" width="70%" />
