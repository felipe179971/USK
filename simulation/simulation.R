library(USK)
library(dplyr)
library(plotly)
library(mice)
taus=c(4,4,-4,-4,9,-9)
Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
erro<-rnorm(3*length(taus),0,1)
y<-2+taus+erro
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

a<-usktest(y~Tratamento,dados,ANOVA=F)
plot_usk(a)
plotly_usk(a)
dados$Treatment<-dados$Tratamento
help("usktest")
############################################################################

dataset<-function(taus,mu,sigma,observations,missings,groups,seed,iterations){
  #Sorteando um tau entre (2 e 10)
  #Tau<-10#sample(2:10,1)
  #Vendo quantos tratamentos terei para cada tau (entre 2 e 5)
  #Tau_Treatment<-Tau/2#sample(2:5,1)
  #Exemplo, se taus=([2,2],[-2,-2]) serão 4 tratamentos em 2 groups
  #if(groups==1){
  #  taus=c(rep(Tau,Tau_Treatment))
  #}
  #if(groups==2){
  #  #taus=c(rep(Tau,Tau_Treatment),rep(-2*Tau,Tau_Treatment))
  #  taus=c(rep(Tau,Tau_Treatment),rep(-Tau,Tau_Treatment))
  #}
  #if(groups==4){
  #  multiplicador<-sample(2:4,1)
  #  taus=c(rep(Tau,Tau_Treatment),rep(-2*Tau,Tau_Treatment),rep(multiplicador*Tau,Tau_Treatment),rep(-2*multiplicador*Tau,Tau_Treatment))
  #}
  #Sorteando mu (entre 1 e 10)
  #mu<-sample(1:Tau,1)

  #Número de tratamentos
  Ntreatment<-length(taus)
  #Vetor Erro
  set.seed(seed)
  erro<-rnorm(observations*Ntreatment,0,sigma)
  y<-mu+taus+erro
  #Vetor que será a coluna "Treatment"
  Treatment<-as.factor(rep(c(seq(1:Ntreatment)),observations))
  #Definindo quem receberá NA
  recebe_NA<-ceiling(observations*Ntreatment*(missings/100))
  set.seed(seed)
  y[sample(1:length(y),recebe_NA,replace=F)]<-NA
  #Banco de dados
  dados<-data.frame(ID=seq(1:length(y)),y,Treatment)
  #Imputação
  imputed<-mice(dados, m=20, method = 'norm',print=F) #ou m=20
  dados<-merge(dados,complete(imputed,)[,-3],by="ID")[,-1]
  dados$original<-mu+taus+erro
  #Identificando
  colnames(dados)<-c("Missing","Treatment","Imputed","Original")
  return(dados)
}
Verificacao<-function(x){
  #Verificando se algum tratamento ficou totalmente vazio
  return(sum(is.na(tapply(x$Missing, x$Treatment,mean,na.rm=T))))
}

simulacao<-function(iterations,alpha,taus,mu,sigma,observations,missings,groups){
  #Vetor vazio que armazenará o resultado
  resultado_O<-c()
  resultado_M<-c()
  resultado_I<-c()
  for(i in 1:iterations){
    dados<-dataset(taus,mu,sigma,observations,missings,groups,seed=i,iterations)
    #Verificando se algum tratamento ficou totalmente vazio
    Verificando<-Verificacao(dados)
    #Refazer o banco até que nenhum tratamento fique só com missing
    j<-1
    while (Verificando>0) {
      print(paste0("Trying again...[missing ",j,"]"))
      #Tentando novamente
      dados<-dataset(taus,mu,sigma,observations,missings,groups,seed=iterations+j,iterations)
      #Verificando se algum tratamento ficou totalmente vazio
      Verificando<-Verificacao(dados)
      print(Verificando)
      j<-j+1
    }
    scott_O<-as.data.frame(usktest(Original~Treatment,dados,alpha,ANOVA=F))[,c(3:8)]
    scott_M<-as.data.frame(usktest(Missing~Treatment,dados,alpha,ANOVA=F))[,c(3:8)]
    scott_I<-as.data.frame(usktest(Imputed~Treatment,dados,alpha,ANOVA=F))[,c(3:8)]

    #Exemplo, se taus=([2,2],[-2,-2]) serão 4 tratamentos em 2 groups
    #Definindo se dividiu na quantidade esperada
    divisao_O<-ifelse(length(levels(as.factor(scott_O$Group)))==groups ,paste0("groups=",groups ),"Diferente")
    divisao_M<-ifelse(length(levels(as.factor(scott_M$Group)))==groups ,paste0("groups=",groups ),"Diferente")
    divisao_I<-ifelse(length(levels(as.factor(scott_I$Group)))==groups ,paste0("groups=",groups ),"Diferente")

    resultado_O[i]<-(divisao_O)
    resultado_M[i]<-(divisao_M)
    resultado_I[i]<-(divisao_I)
    print(dados)
  }

  return(list(Original=c(prop.table(table(resultado_O))),Missing=c(prop.table(table(resultado_M))),Imputed=c(prop.table(table(resultado_I)))))

}
###############
#Definindo as porcentagens
percentage<-c(0,5,10,15,20,25,30)
#percentage<-c(0)
#Criando lista para armazenar os resultados
result<-as.list(1:length(percentage))
#Executando
for (i in 1:length(percentage)){

  result[[i]]<-simulacao(
    #Quantidade de casos a serem avaliados (iterações)
    iterations=1000,
    #Erro Tipo I
    alpha=0.05,
    #Definindo os valores dos taus e quantos tratamentos terei
    taus=c(40,30,20,10,-10,-20,-30,-40),
    #Média
    mu=20,
    #Sigma do modelo erro~N(0,sigma)
    sigma=1,
    #Número de observações (ou blocos) em cada tratamentos
    observations=10,
    #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
    missings=percentage[i],
    #Quantidades de grupos que o teste deveria retornar
    groups=8
  )
  names(result)[i]<-paste0("Missings= ",percentage[i],"%")
  #Avaliando o progresso
  print(paste0(i," (",round(i/length(percentage)*100),"%)"))
}

#Olhando o resultado
V19<-result

#Pegando só o que acertou
quero<-V19
resultado_original<-c()
for (i in 1:length(quero)) {
  resultado_original[i]<-quero[[i]][[1]][["groups=8"]]
}

resultado_missing<-c()
for (i in 1:length(quero)) {
  resultado_missing[i]<-quero[[i]][[2]][["groups=8"]]
}

resultado_imputed<-c()
for (i in 1:length(quero)) {
  resultado_imputed[i]<-quero[[i]][[3]][["groups=8"]]
}

data.frame(percentage=percentage/100,original=resultado_original,
           missing=resultado_missing,
           imputed=resultado_imputed)
############################
(D<-dataset(taus=c(10,-10),mu=10,sigma=1,observations=10,missings=30,groups=2,seed=1,iterations=1))
simulacao(iterations=1,alpha=0.05,taus=c(10,-10),mu=10,sigma=1,observations=4,missings=30,groups=2)
8*.3

A<-D
usktest(Missing~Treatment,A,alpha=0.05)
plotly_usk(usktest(Missing~Treatment,A,alpha=0.05,ANOVA=F))
plotly_usk(usktest(Imputed~Treatment,A,alpha=0.05,ANOVA=F))
plotly_usk(usktest(Original~Treatment,A,alpha=0.05,ANOVA=F))

set.seed(1)
rnorm(2)

set.seed(2)
sample(25,1)
