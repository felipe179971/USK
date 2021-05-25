#' U
#'
#' @description \code{\link{usktest}} is a function used to do the Scott-Knott
#' cluster analyses (1974) for unbalanced designs proposed at 2017 (CONRADO, Thiago
#' Vincenzi et al). To learn more, see the \href{http://ref.scielo.org/ws792m}{article}.
#'
#' @param test ...
#'
#' @import purrr dplyr
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous geom_errorbar theme_bw labs
#' @importFrom pkgcond suppress_messages
#' @importFrom plotly ggplotly layout
#' @importFrom stats aov qchisq as.formula
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com}
#'
#' @examples
#'
#' taus=c(4,4,-4,-4,9,-9)
#' Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
#' erro<-rnorm(3*length(taus),0,1)
#' y<-2+taus+erro
#' y[round(runif(1,min=1,max=length(y)),0)]<-NA
#' dados<-data.frame(y,Tratamento)
#'
#' test<-usktest(y~Tratamento,dados)
#' plotly_usk(test)
#' @return This function returns the ANOVA table, a graph and a \code{data.frame} including columns:
#' \itemize{
#'  \item '\code{treatments}': treatments.
#'  \item \code{group}: which group the treatment was classified.
#'  \item \code{mean}: average value of 'observation' for each treatment.
#'  \item \code{min}: lowest value of 'observation' for each treatment.
#'  \item \code{max}: higher value of 'observation' for each treatment.
#' }
#'
#' @encoding UTF-8
#' @export

plotly_usk <-
function(test){
    ggplot_usk<-plot_usk(test)
    graphic<-
      ggplotly(ggplot_usk,tooltip = c("Treatment","x","Group"))%>%
      layout(title = "Scott-Knott",
             xaxis = list(title = "Mean",titlefont=list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")),
             yaxis = list(title = ""),
             showlegend = T)
    return(graphic)
  }
