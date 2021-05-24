#' @title ...
#' @name Graphic
#'
#' @description ...
#'
#' @param R ...
#'
#' @author Felipe Ferreira
Graphic <-
function(R){
  RR<-R%>%
    ggplot(aes(Treatment=Treatment,y=Ordem, x=`Mean`,color=Group)) +
    geom_point() +
    scale_y_continuous(breaks=c(R$Ordem),labels=c(as.character(R$Treatment)),name="")+
    geom_errorbar(aes(xmin=min,xmax=max),width = 0.2,size  = 0.7) +
    theme_bw()+
    labs(title = "Scott-Knott")

  Grafico<-ggplotly(RR,tooltip = c("Treatment","x","Group"))%>%
    layout(title = "Scott-Knott",
           xaxis = list(title = "Mean",titlefont=list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")),
           yaxis = list(title = ""),
           showlegend = FALSE)
  print(Grafico)
}
