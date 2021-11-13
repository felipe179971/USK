#' @title Unbalanced Scott-Knott (USK)
#'
#' @name usktest
#'
#' @description \code{\link{usktest}} is a function used to do the Scott-Knott
#' cluster analysis (1974) for unbalanced designs proposed at 2017 (CONRADO, T. V; FERREIRA, D. F.; SCAPIM, C. A.; MALUF, W. R.).
#'  To learn more, see the \href{http://ref.scielo.org/ws792m}{article}.
#'
#' @param formula at the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be \code{observation ~ treatment}
#' @param dataset an indication to the database being used.
#' @param alpha type I error the researcher can accept (the default is 0.05).
#' @param ANOVA if TRUE (the default), print an ANOVA table.
#'
#' @return If \code{ANOVA=TRUE}, this function returns the ANOVA table and the Scott-Knott table that are a \code{data.frame} including 5 columns:
#' \itemize{
#'  \item '\code{treatments}': the name of the 'treatments' parameter.
#'  \item \code{group}: which group the treatment was classified.
#'  \item \code{mean}: average value of 'observation' for each treatment.
#'  \item \code{min}: lowest value of 'observation' for each treatment.
#'  \item \code{max}: higher value of 'observation' for each treatment.
#' }
#'
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com}
#'
#' @references CONRADO, T. V; FERREIRA, D. F.; SCAPIM, C. A.; MALUF, W. R. (2017) \emph{Adjusting the Scott-Knott cluster analyses for unbalanced designs}.
#'
#' @seealso For graphics: \code{plot_usk} or \code{plotly_usk}. See also the packages \href{https://cran.r-project.org/web/packages/ScottKnott/index.html}{ScottKnott}, \href{https://cran.r-project.org/web/packages/ScottKnottESD/index.html}{ScottKnottESD} and \href{https://cran.r-project.org/web/packages/multcomp/index.html}{multcomp}.
#'
#' @examples
#'
#' set.seed(3)
#' taus=c(4,4,-4,-4,9,-9)
#' Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
#' erro<-rnorm(3*length(taus),0,1)
#' y<-2+taus+erro
#' y[round(runif(1,min=1,max=length(y)),0)]<-NA
#' dados<-data.frame(y,Tratamento)
#'
#' test<-usktest(y~Tratamento,dados)
#'
#' #ggplot2
#' plot_usk(test)
#' #plotly
#' #plotly_usk(test)
#'
#'
#' @import purrr dplyr
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous geom_errorbar theme_bw labs
#' @importFrom pkgcond suppress_messages
#' @importFrom plotly ggplotly layout
#' @importFrom stats aov qchisq as.formula
#'
#'
#'
#' @encoding UTF-8
#' @export

usktest <-
  function(formula,dataset,alpha=0.05,ANOVA=TRUE){
    var1<-as.character(formula[[2]])
    var2<-as.character(formula[[3]])
    #Checking for possible errors
    if(length(as.character(formula[[3]]))>1){
      verifications<-list(c(length(as.character(formula[[3]]))>1,"At the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be 'observation ~ treatment'"))
    }else{

      if(is.factor(dataset[[var2]])==FALSE){
                    warning(paste0("The variable '",as.character(formula[[3]]),"' has been changed to format 'factor'"))
                    dataset[[var2]]<-as.factor(dataset[[var2]])
      }
      verifications<-list(c(is.numeric(dataset[[var1]])==FALSE,paste0("The variable '",as.character(formula[[2]]),"' must be numeric")),
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
      #Running the Modified Skott-Knott Test
      Result<-RUNscott(dataset,var1,var2,alpha,ANOVA(formula,dataset)[["df.residual"]],summary(ANOVA(formula,dataset))[[1]][["Mean Sq"]][[2]])
      colnames(Result)[1]<-var2

      Resultado<-as.list(c(var1,var2,Result))
      names(Resultado)[1:2] <- c("Variable of observations", "Variable of treatment")
      #Running the ANOVA Test (if the user wants to)
      if(ANOVA==T){
        Anova<-summary(ANOVA(formula,dataset))
        print("##########################ANOVA###########################")
        print(Anova)
        print("#######################Scott-Knott########################")
        Resultado<-as.list(c(var1,var2,Anova,Result))
        names(Resultado)[1:3] <- c("Variable of observations", "Variable of treatment", "ANOVA")
      }
      #Printing the results in the form of tables
      print(subset(Result, select=-c(Ordem)))
      #Returning results in unprinted 'list ′ format
      invisible(Resultado)
    }
  }

