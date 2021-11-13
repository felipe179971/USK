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
  #Running the test for all possible partitions and sorting the groups
  #Group 1, 2, ..., up to how many groups there are
  while(i <=max(Group)){
    #If there is more than 1 treatment in the Group: take the test
    if(length(unique(dados[which(dados$Group==i),]$Treatment))>1){
      #Running the modified scott−knott test for the i-th group
      Segundo<-scott(dados[which(dados$Group==i),],var1,var2,alpha,v,MSE)
      #If H0 is rejected: assign the Groups
      if(Segundo[[1]][[1]]==TRUE){
        #It will be assigned with different number than any existing one
        Group<-c(as.numeric(max(dados$Group))+1,as.numeric(max(dados$Group))+2)
        #The code splits into 2 groups (Makes to 1 and then to 2)
        for(j in 1:2){
          #Get from the first to the last treatment belonging to this group
          for (k in 1:length(Segundo[[2]][[j]])) {
            #Look at all the lines of the data to see if in the nr−th line we have the treatment
            for(nr in 1:nrow(dados)){
              #Put the group on the line of your treatment
              if(is.element(dados$Treatment[nr], names(Segundo[[2]][[j]]))==TRUE){
                dados$Group[nr]<-Group[j]
              }
            }
          }
        }
        #If H0 was rejected, the "while" is updated
        Group<-as.numeric(levels(as.factor(dados$Group)))
      }
    }
    #Updating the 'i' of the "while"
    i<-i+1
  }
  #Table with the data
  #Looking at the result with suppress messages
  suppress_messages(
    R<-dados %>%group_by(Treatment,Group)%>%
      summarise(`Mean`=round(mean(y,na.rm=TRUE),2),
                min=min(y,na.rm=TRUE),
                max=max(y,na.rm=TRUE))%>%
      arrange(desc(`Mean`)))
  #'Ordem' will only be used in the chart (do not print to the user)
  R$Ordem<-as.numeric(row.names(R))
  #Transforming from number to letter
  R$Group<-c(number_to_letters(as.numeric(R$Group)))
  #In the graph it is important that the group is a factor
  R$Group<-as.factor(R$Group)
  return(R)
}
