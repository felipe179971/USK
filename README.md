
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
#> Tratamento   5  630.6   126.1   115.1 4.08e-09 ***
#> Residuals   11   12.0     1.1                     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 1 observation deleted due to missingness
#> [1] "#######################Scott-Knott########################"
#> # A tibble: 6 x 5
#> # Groups:   Tratamento [6]
#>   Tratamento Group  Mean   min   max
#>   <fct>      <fct> <dbl> <dbl> <dbl>
#> 1 trat 5     a     11.1  10.2  12.0 
#> 2 trat 1     b      5.43  3.46  7.43
#> 3 trat 2     b      5.04  3.96  5.68
#> 4 trat 3     c     -2.83 -3.15 -2.56
#> 5 trat 4     c     -3.01 -3.49 -2.53
#> 6 trat 6     d     -6.13 -6.37 -5.85
```

<img src="man/figures/README-example-1.png" width="90%" />
