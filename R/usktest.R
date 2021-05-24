#' Unbalanced Scott-Knott (USK)
#'
#' @description \code{\link{usktest}} is a function used to do the Scott-Knott
#' cluster analyses (1974) for unbalanced designs proposed at 2017 (CONRADO, Thiago
#' Vincenzi et al). To learn more, see the \href{http://ref.scielo.org/ws792m}{article}
#'
#' @param dataset An indication to the database being used.
#' @param var1 A quantitative variable containing the metric results of the experiment.
#' @param var2 A factor variable indicating the treatments.
#' @param alpha Type I error the researcher can accept (the default is 0.05).
#' @param graphic If TRUE (the default), export a chart ggplotly.
#' @param ANOVA If TRUE (the default), export an ANOVA table.
#'
#' @import purrr dplyr
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous geom_errorbar theme_bw labs
#' @importFrom pkgcond suppress_messages
#' @importFrom plotly ggplotly layout
#' @importFrom stats aov qchisq
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
#' usktest(dados,"y","Tratamento",0.05,graphic=TRUE,ANOVA=TRUE)
#'
#' @return This function returns the ANOVA table, a graph and a \code{data.frame} including columns:
#' \itemize{
#'  \item "var2": Treatments.
#'  \item group: Which group the treatment was classified.
#'  \item mean: Average value of "var1" for each treatment.
#'  \item min: Lowest value of "var1" for each treatment.
#'  \item max Higher value of "var1" for each treatment.
#' }
#'
#' @encoding UTF-8
#' @export

usktest <-
function(dataset,var1,var2,alpha,graphic,ANOVA){
  if(is.factor(dataset[[var2]])==FALSE){
    stop('The argument "var2" must be factor')
  }else if(is.numeric(dataset[[var1]])==FALSE){
    stop('The argument "var1" must be numeric')
  }else if(length(unique(dataset[[var2]]))<2){
    stop('The variable "var2" must have more than 1 treatment')
  }else if(max(table(dataset[[var2]]))<2){
    stop('The variable "var2" must have more than 1 observations')
  }else{
    if(graphic==T){
      Graphic(as.data.frame(RUNscott(dataset,var1,var2,alpha,ANOVA(dataset,var1,var2)[["df.residual"]],summary(ANOVA(dataset,var1,var2))[[1]][["Mean Sq"]][[2]])))
    }
    if(ANOVA==T){
      Anova<-data.frame(summary(ANOVA(dataset,"y","Tratamento"))[[1]])
      rownames(Anova)[1]<-var2
      print("##########################ANOVA###########################")
      print(Anova)
      print("#######################Scott-Knott########################")

    }
    Result<-RUNscott(dataset,var1,var2,alpha,ANOVA(dataset,var1,var2)[["df.residual"]],summary(ANOVA(dataset,var1,var2))[[1]][["Mean Sq"]][[2]])[,-6]
    colnames(Result)[1]<-var2
    return(Result)
  }
}

