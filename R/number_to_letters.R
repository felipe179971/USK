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
    #Set the maximum number of letters, where the alphabet repeats every 26 numbers
    for (r in 1:repet){
      letras<-letras
      reset<-1
      #1 to 26: ”a",...,”z"; 27 to 52: ”aa",...,”zz"; 53 to 78: ”aaa",...,”zzz";...
      for(l in (26*r-25):(26*r)){
        letras[l]<-paste0(c(as.character(rep(letters[reset],r))),collapse = "")
        reset<-reset+1
      }
    }
    #Sorting the (i + 1)-th element
    for (i in 1:(n-1)) {
      #The first group receives the first letter ("a")
      modificado[1]<-letras[1]
      #The following treatments receive the same letter as the previous one (if they are from the same group)
      if(a[i+1]==a[i]){
        modificado[i+1]<-modificado[i]
        j<-j
      #The following treatments receive the next letter (if they are not from the same group as the previous one)
      }else{
        modificado[i+1]<-letras[j+1]
        j<-j+1
      }
    }
    return(modificado)
  }

