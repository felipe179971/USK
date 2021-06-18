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
    j<-1;n<-length(a);
    modificado<-c();letras<-c()
    repet<-ceiling(n/26)
    for (r in 1:repet){
      letras<-letras
      reset<-1
      for(l in (26*r-25):(26*r)){
        letras[l]<-paste0(c(as.character(rep(letters[reset],r))),collapse = "")
        reset<-reset+1
      }
    }
    for (i in 1:(n-1)) {
      modificado[1]<-letras[1]
      if(a[i+1]==a[i]){
        modificado[i+1]<-modificado[i]
        j<-j
      }else{
        modificado[i+1]<-letras[j+1]
        j<-j+1
      }
    }
    return(modificado)
  }

