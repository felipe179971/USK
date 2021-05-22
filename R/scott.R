#' @title ...
#' @name scott
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
scott <-
function(dataset,var1,var2,alpha,v,MSE){
  yi<-sort(tapply(dataset[[var1]],dataset[[var2]],mean,na.rm=TRUE),decreasing=TRUE)
  bar_y<-mean(yi)
  k<-length(yi)
  length.na.rm <- function(x){sum(!is.na(x))}
  s2_c<-mean(MSE/tapply(dataset[[var1]],dataset[[var2]],length.na.rm), na.rm=TRUE)
  sub<-as.list(1:(k-1))
  subG<-map(sub, function(sub){list(yi[c(1:sub)], yi[-c(1:sub)])})
  B0<-as.list(map(as.numeric(sub),function(x) {
    T1<-sum(subG[[x]][[1]])
    T2<-sum(subG[[x]][[2]])
    k1<-length(subG[[x]][[1]])
    k2<-length(subG[[x]][[2]])
    list( (T1^2/k1) + (T2^2/k2) - (sum(c(T1,T2))^2)/sum(c(k1,k2)),
          subG[[x]])
  }))
  B0max<-B0[which.max(map(B0,1))][[1]][[1]]
  SubB0max<-B0[which.max(map(B0,1))][[1]][[-1]]
  sigma<-(sum((yi-bar_y)^2)+(v*s2_c))/(k+v)
  lambda<-(pi/(2*(pi-2)))*(B0max/sigma)
  limite<-qchisq((1-alpha), df = k/(pi - 2))
  rejeita<-ifelse(lambda>limite, TRUE, FALSE)
  return( list(rejeita=rejeita,SubB0max=SubB0max) )
}
