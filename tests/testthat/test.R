
####Banco de dados "Normal"########
set.seed(17)
taus=c(4,4,-4,-4,9,-9)
Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
erro<-rnorm(3*length(taus),0,1)
y<-2+taus+erro
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

####Variáveis para teste##########
#y=Fator: The variable 'y' must be numeric####
dados$y_factor<-as.factor(dados$y)
#Tratamento=character: The variable 'Tratamento' must be factor####
dados$Tratamento_character<-as.character(dados$Tratamento)
#trat 1=100%NA:  All 'Tratamento' must have more than 1 observations####
dados$y_1var_NA<-dados$y;dados$y_1var_NA[dados$Tratamento=="trat 1"]<-NA
#Tratamento=trat 1:  The variable 'Tratamento' must have more than 1 type of treatment####
dados$Tratamento_unico<-as.factor(c("trat 1"))
#Testando o alpha
resultado_005<-data.frame(
  Tratamento=as.factor(c("trat 5","trat 2","trat 1","trat 3","trat 4","trat 6")),
  Group=as.factor(c("a","b","b","c","c","d")),
  Mean=c(11.9 ,6.61,5.98,-1.46,-2.17,-6.79),
  min=c(11.8 ,5.92,4.98,-2.23,-2.82,-7.17),
  max=c(12.2  ,7.72 ,6.97 ,-0.409,-1.63 ,-6.36 )
)
resultado_099<-data.frame(
  Tratamento=as.factor(c("trat 5","trat 2","trat 1","trat 3","trat 4","trat 6")),
  Group=as.factor(c("a","b","c","d","e","f")),
  Mean=c(11.9 ,6.61,5.98,-1.46,-2.17,-6.79),
  min=c(11.8 ,5.92,4.98,-2.23,-2.82,-7.17),
  max=c(12.2  ,7.72 ,6.97 ,-0.409,-1.63 ,-6.36 )
)
#Testando o gráfico
verificar_grafico<-function(alpha){
  grafico<-usktest(y~Tratamento,dados,ANOVA=F,graphic=T,alpha=alpha)
  colnames(grafico)[1]<-"Treatment"
  grafico$Ordem<-c(1:6)
  return(USK:::Graphic(grafico)$x$data[[3]]$text[1])
}
grafico_005<-verificar_grafico(0.05)
grafico_099<-verificar_grafico(0.99)

####TESTE##########
expect_error(usktest(y_factor~Tratamento,dados),"The variable 'y_factor' must be numeric")
expect_error(usktest(y~Tratamento_character,dados),"The variable 'Tratamento_character' must be factor")
expect_error(usktest(y_1var_NA~Tratamento,dados),"All 'Tratamento' must have more than 1 observations")
expect_error(usktest(y~Tratamento_unico,dados),"The variable 'Tratamento_unico' must have more than 1 type of treatment")
expect_error(usktest(y~Tratamento+Tratamento_unico,dados),"At the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be 'observation ~ treatment'")
expect_equal(capture.output(usktest(y~Tratamento,dados,ANOVA=F,graphic=F))[1],"\033[38;5;246m# A tibble: 6 x 5\033[39m")
expect_equal(capture.output(usktest(y~Tratamento,dados,graphic=F))[1],"[1] \"##########################ANOVA###########################\"")
expect_equal(capture.output(usktest(y~Tratamento,dados))[3],"Tratamento   5 710.660096 142.1320191 230.4221 9.573813e-11")
expect_equal(grafico_005,"Mean: -1.46<br />Treatment: trat 3<br />Group: c")
expect_equal(grafico_099,"Mean:  5.98<br />Treatment: trat 1<br />Group: c")

expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F)$Tratamento,resultado_005$Tratamento)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F)$Group,resultado_005$Group)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F)$Mean,resultado_005$Mean,tolerance=0.05)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F)$min,resultado_005$min,tolerance=0.05)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F)$max,resultado_005$max,tolerance=0.05)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F,alpha=0.99)$Tratamento,resultado_099$Tratamento)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F,alpha=0.99)$Group,resultado_099$Group)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F,alpha=0.99)$Mean,resultado_099$Mean,tolerance=0.05)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F,alpha=0.99)$min,resultado_099$min,tolerance=0.05)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,graphic=F,alpha=0.99)$max,resultado_099$max,tolerance=0.05)


a<-usktest(y~Tratamento,dados,ANOVA=F,graphic=T)
colnames(a)[1]<-"Treatment"
a$Ordem<-c(1:6)

aa<-USK:::Graphic(a)$x$data[[1]]$text


