#' @title ...
#' @name ANOVA
#'
#' @description ...
#'
#' @param dataset ...
#' @param formula ...
#'
#'
#' @author Felipe Ferreira

ANOVA <-
function(formula,dataset){
  modelo<-aov(as.formula(formula),dataset)
  return(modelo)
}
