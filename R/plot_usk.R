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
#'
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

plot_usk <-
function(test){
    data<-as.data.frame(test[c(test[[2]],"Group","Mean","min","max","Ordem")])
    colnames(data)[1]<-c("Treatment")
    graphic<-data%>%
      ggplot(aes(Treatment=Treatment,y=Ordem, x=`Mean`,color=Group)) +
      geom_point() +
      scale_y_continuous(breaks=c(data$Ordem),labels=c(as.character(data[,1])),name=test[[2]])+
      geom_errorbar(aes(xmin=min,xmax=max),width = 0.2,size  = 0.7) +
      theme_bw()+
      labs(title = "Scott-Knott")
    return(graphic)
  }
