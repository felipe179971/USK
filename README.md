
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
#> Tratamento   5  653.2  130.64   92.82 1.29e-08 ***
#> Residuals   11   15.5    1.41                     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 1 observation deleted due to missingness
#> [1] "#######################Scott-Knott########################"
#> # A tibble: 6 x 5
#> # Groups:   Tratamento [6]
#>   Tratamento Group  Mean   min    max
#>   <fct>      <fct> <dbl> <dbl>  <dbl>
#> 1 trat 5     a     11.9  11.3  12.3  
#> 2 trat 2     b      6.01  5.93  6.09 
#> 3 trat 1     b      5.18  2.71  7.66 
#> 4 trat 3     c     -1.24 -1.73 -0.654
#> 5 trat 4     c     -1.91 -2.31 -1.33 
#> 6 trat 6     d     -6.53 -7.11 -5.51
```

<img src="man/figures/README-example-1.png" width="100%" />
