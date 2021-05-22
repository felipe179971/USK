#' @title ...
#' @name number_to_letters
#'
#' @description ...
#'
#' @param a ...
#'
#' @author Felipe Ferreira
number_to_letters <-
function(a){
  j<-1
  modificado<-c()
  for (i in 1:(length(a)-1)) {
    letras<-letters[1:(length(a)-1)]
    modificado[1]<-letters[1]
    if(a[i+1]==a[i]){
      modificado[i+1]<-modificado[i]
      j<-j
    }else{
      modificado[i+1]<-letters[j+1]
      j<-j+1
    }
  }
  return(modificado)
}
