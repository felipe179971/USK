
<!-- README.md is generated from README.Rmd. Please edit that file -->

# USK

<!-- badges: start -->
<!-- badges: end -->

(Descrição)

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

usktest(y~Tratamento,dados,ANOVA = F)
#> # A tibble: 6 x 5
#> # Groups:   Tratamento [6]
#>   Tratamento Group  Mean   min    max
#>   <fct>      <fct> <dbl> <dbl>  <dbl>
#> 1 trat 5     a     10.7  10.1  11.2  
#> 2 trat 1     b      5.96  4.98  6.97 
#> 3 trat 2     b      5.14  4.81  5.74 
#> 4 trat 4     c     -0.94 -2.55  0.103
#> 5 trat 3     d     -3.48 -4.25 -2.89 
#> 6 trat 6     e     -7.94 -8.58 -7.36
```
