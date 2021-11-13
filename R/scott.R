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
  #Function to obtain the 'n' of each Treatment without counting the 'NA'
  length.na.rm <- function(x){sum(!is.na(x))}
  #S2 modified by Conrado: s_c^2=\frac{1}{k}\sum_{i=1}^k(SE_{\bar{y}_i})
  s2_c<-mean(MSE/tapply(dataset[[var1]],dataset[[var2]],length.na.rm), na.rm=TRUE)
  #List with the number of possible subgroups (k−1)
  sub<-as.list(1:(k-1))
  #Creating the subgroups
  subG<-map(sub, function(sub){list(yi[c(1:sub)], yi[-c(1:sub)])})
  #Calculating B0 for each of the subgroups
  B0<-as.list(map(as.numeric(sub),function(x) {
    #Defining
    T1<-sum(subG[[x]][[1]])
    T2<-sum(subG[[x]][[2]])
    k1<-length(subG[[x]][[1]])
    k2<-length(subG[[x]][[2]])
    #Formula of B_{0}=\frac{T_1^2}{k_1}+\frac{T_2^2}{k_2}-\frac{(T_1+T_2)^2}{k_1+k_2}
    list( (T1^2/k1) + (T2^2/k2) - (sum(c(T1,T2))^2)/sum(c(k1,k2)),
          subG[[x]])
  }))
  #Getting the biggest B0
  B0max<-B0[which.max(map(B0,1))][[1]][[1]]
  #Getting the set that gives the biggest B0
  SubB0max<-B0[which.max(map(B0,1))][[1]][[-1]]
  #Calculating the sigma: \widehat{\sigma}_0^2=\frac{[\sum_{i=1}^k(y_i-\bar{y})^2+vs^2]}{k+v};\;s^2=\frac{MS_E}{n}
  sigma<-(sum((yi-bar_y)^2)+(v*s2_c))/(k+v)
  #Calculating the lambda: \lambda=\frac{\pi}{2(\pi-2)}*\frac{B_0}{\widehat{\sigma}_0^2}
  lambda<-(pi/(2*(pi-2)))*(B0max/sigma)
  #Calculating the Chi-Square: \chi^2_{\alpha,\frac{k}{(\pi-2)}}
  limite<-qchisq((1-alpha), df = k/(pi - 2))
  #reject H_0 if \lambda > \chi^2_{\alpha,\frac{k}{(\pi-2)}}
  rejeita<-ifelse(lambda>limite, TRUE, FALSE)
  #Output
  return( list(rejeita=rejeita,SubB0max=SubB0max) )
}
