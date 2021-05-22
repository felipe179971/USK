#' @title ...
#' @name RUNscott
#'
#' @description ...
#'
#' @param dataset ...
#' @param var1 ...
#' @param var2 ...
#' @param alpha ...
#' @param v ...
#' @param MSE ...
#'
#' @author Felipe Ferreira
RUNscott <-
function(dataset,var1,var2,alpha,v,MSE){
  dados<-dataset
  dados$Treatment<-dataset[[var2]]
  dados$y<-dataset[[var1]]
  i<-1;Group<-1
  dados$Group<-1
  while(i <=max(Group)){
    if(length(unique(dados[which(dados$Group==i),]$Treatment))>1){
      Segundo<-scott(dados[which(dados$Group==i),],var1,var2,alpha,v,MSE)
      if(Segundo[[1]][[1]]==TRUE){
        Group<-c(as.numeric(max(dados$Group))+1,as.numeric(max(dados$Group))+2)
        for(j in 1:2){
          for (k in 1:length(Segundo[[2]][[j]])) {
            for(nr in 1:nrow(dados)){
              if(is.element(dados$Treatment[nr], names(Segundo[[2]][[j]]))==TRUE){
                dados$Group[nr]<-Group[j]
              }
            }
          }
        }
        Group<-as.numeric(levels(as.factor(dados$Group)))
      }
    }
    i<-i+1
  }
  suppress_messages(
    R<-dados %>%group_by(Treatment,Group)%>%
      summarise(`Mean`=round(mean(y,na.rm=TRUE),2),
                min=min(y,na.rm=TRUE),
                max=max(y,na.rm=TRUE))%>%
      arrange(desc(`Mean`)))
  R$Ordem<-as.numeric(row.names(R))
  R$Group<-c(number_to_letters(as.numeric(R$Group)))
  R$Group<-as.factor(R$Group)
  return(R)
}
