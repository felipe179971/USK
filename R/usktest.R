#' ...
#'
#' ...
#'
#' @param dataset ...
#' @param var1 ...
#' @param var2 ...
#' @param alpha ...
#' @param graphic ...
#' @param ANOVA ...
#'
#' @import purrr dplyr plotly
#' @importFrom pkgcond suppress_messages
#' @importFrom ggplot2 ggplot
#' @importFrom stats aov qchisq
#'
#'
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
      Anova<-data.frame(summary(ANOVA(dados,"y","Tratamento"))[[1]])
      rownames(Anova)[1]<-var2
      print("##########################ANOVA###########################")
      print(Anova)
      print("#######################Scott-Knott########################")

    }
    return(RUNscott(dataset,var1,var2,alpha,ANOVA(dataset,var1,var2)[["df.residual"]],summary(ANOVA(dataset,var1,var2))[[1]][["Mean Sq"]][[2]])[,-6])
  }
}
