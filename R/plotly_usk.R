#' @title Unbalanced Scott-Knott (USK) on a Plotly Graphic
#'
#' @name plotly_usk
#'
#' @description \code{\link{plotly_usk}} is used to show the \code{\link{usktest}} result,
#' that do the Scott-Knott cluster analyses (1974) for unbalanced designs proposed at 2017
#'  (CONRADO, Thiago Vincenzi et al), in a plotly graphic.
#'  To learn more, see the \href{http://ref.scielo.org/ws792m}{article}.
#'
#' @param test  is the \code{\link{plotly_usk}} function.
#'
#' @return This function returns a plotly graph with:
#' \itemize{
#'  \item \code{title}: "Scott-Knott";
#'  \item \code{x}:  treatments;
#'  \item \code{y}:   average value of 'observation' for each treatment;
#'  \item \code{geom_errorbar}: lowest to higher values of 'observation' for each treatment;
#'  \item \code{color}: which group the treatment was classified.
#' }
#'
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com}
#'
#' @references CONRADO, T. V; FERREIRA, D. F.; SCAPIM, C. A.; MALUF, W. R. (2017) \emph{Adjusting the Scott-Knott cluster analysis for unbalanced designs}.
#'
#' @seealso For another graphic: \code{plot_usk}. See also the packages \href{https://cran.r-project.org/web/packages/ScottKnott/index.html}{ScottKnott}, \href{https://cran.r-project.org/web/packages/ScottKnottESD/index.html}{ScottKnottESD} and \href{https://cran.r-project.org/web/packages/multcomp/index.html}{multcomp}.
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
#' #plotly
#' plotly_usk(test)
#' #ggplot2
#' plot_usk(test)
#'
#' @import purrr dplyr
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous geom_errorbar theme_bw labs
#' @importFrom pkgcond suppress_messages
#' @importFrom plotly ggplotly layout
#' @importFrom stats aov qchisq as.formula
#'
#' @encoding UTF-8
#' @export

plotly_usk <-
function(test){
    #Using ggplot2 as a base
    ggplot_usk<-plot_usk(test)
    #Transforming ggplot2 into plotly
    graphic<-
      ggplotly(ggplot_usk,tooltip = c("Treatment","x","Group"))%>%
      #Centering the title
      layout(title = "Scott-Knott",
             #Customizing the labels and caption
             xaxis = list(title = "Mean",titlefont=list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")),
             yaxis = list(title = ""),
             showlegend = T)
    #Returning the graphic
    return(graphic)
  }
