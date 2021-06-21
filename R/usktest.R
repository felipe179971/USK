#' Unbalanced Scott-Knott (USK)
#'
#' @description \code{\link{usktest}} is a function used to do the Scott-Knott
#' cluster analyses (1974) for unbalanced designs proposed at 2017 (CONRADO, Thiago
#' Vincenzi et al). To learn more, see the \href{http://ref.scielo.org/ws792m}{article}.
#'
#' @param formula at the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be \code{observation ~ treatment}
#' @param dataset an indication to the database being used.
#' @param alpha type I error the researcher can accept (the default is 0.05).
#' @param graphic if TRUE (the default), print a chart ggplotly.
#' @param ANOVA if TRUE (the default), print an ANOVA table.
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
#' #Plotly
#' plotly_usk(test)
#' #GGplot2
#' plot_usk(test)
#' #plotly_usk(test)
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

usktest <-
  function(formula,dataset,alpha=0.05,ANOVA=TRUE){
    var1<-as.character(formula[[2]])
    var2<-as.character(formula[[3]])
    if(length(as.character(formula[[3]]))>1){
      verifications<-list(c(length(as.character(formula[[3]]))>1,"At the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be 'observation ~ treatment'"))
    }else{
      verifications<-list(c(is.factor(dataset[[var2]])==FALSE,paste0("The variable '",as.character(formula[[3]]),"' must be a factor")),
                          c(is.numeric(dataset[[var1]])==FALSE,paste0("The variable '",as.character(formula[[2]]),"' must be numeric")),
                          c(length(unique(dataset[[var2]]))<2,paste0("The variable '",as.character(formula[[3]]),"' must have more than 1 type of treatment")),
                          c(sum(tapply(dataset[[var1]],dataset[[var2]],function(x){sum(is.na(x))})==table(dataset[[var2]]))>0,paste0("All '",as.character(formula[[3]]),"' must have more than 1 observations"))
      )
    }

    message<-as.character("")
    count_errors<-0
    for(i in 1:length(verifications)){
      message<-ifelse(verifications[[i]][[1]]==TRUE,paste(message,as.character(verifications[[i]][[2]]),sep="\n"),message)
      count_errors<-ifelse(verifications[[i]][[1]]==TRUE,count_errors+1,count_errors)
    }

    if(count_errors>0){
      stop(paste(message))
    }else{
      Result<-RUNscott(dataset,var1,var2,alpha,ANOVA(formula,dataset)[["df.residual"]],summary(ANOVA(formula,dataset))[[1]][["Mean Sq"]][[2]])
      colnames(Result)[1]<-var2

      Resultado<-as.list(c(var1,var2,Result))
      if(ANOVA==T){
        Anova<-summary(ANOVA(formula,dataset))
        print("##########################ANOVA###########################")
        print(Anova)
        print("#######################Scott-Knott########################")
        Resultado<-as.list(c(var1,var2,Anova,Result))
      }
      print(Result[,-6])
      invisible(Resultado)
    }
  }

