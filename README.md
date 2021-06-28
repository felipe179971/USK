
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

%\`\`\`{r example} library(USK) \#\# basic example code
taus=c(4,4,-4,-4,9,-9)
Tratamento&lt;-as.factor(rep(c(paste(“trat”,seq(1:length(taus)))),3))
erro&lt;-rnorm(3\*length(taus),0,1) y&lt;-2+taus+erro
y\[round(runif(1,min=1,max=length(y)),0)\]&lt;-NA
dados&lt;-data.frame(y,Tratamento)

\#usktest(y\~Tratamento,dados,ANOVA = F) \`\`\`
