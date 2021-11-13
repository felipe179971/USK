
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
#Testando os plots
teste_005<-usktest(y~Tratamento,dados,ANOVA=F)
teste_099<-usktest(y~Tratamento,dados,ANOVA=F,alpha=0.99)
#Testando nomes diferentes das variáveis
dados$Tratamento_nome_diferente<-as.factor(dados$Tratamento)
dados$y_nome_diferente<-as.numeric(dados$y)


####TESTE##########
#Erros
expect_error(usktest(y_factor~Tratamento,dados),"The variable 'y_factor' must be numeric")
expect_warning(usktest(y~Tratamento_character,dados),"The variable 'Tratamento_character' has been changed to format 'factor'")
expect_warning(expect_equal(usktest(y~Tratamento_character,dados)[["ANOVA"]][["Pr(>F)"]][1],9.573813e-11))
expect_error(usktest(y_1var_NA~Tratamento,dados),"All 'Tratamento' must have more than 1 observations")
expect_error(usktest(y~Tratamento_unico,dados),"The variable 'Tratamento_unico' must have more than 1 type of treatment")
expect_error(usktest(y~Tratamento+Tratamento_unico,dados),"At the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be 'observation ~ treatment'")
expect_error(usktest(y_factor~Tratamento+Tratamento_unico,dados),"At the moment, this package only does the Unbalanced Scott-Knott for single factor analysis of variance, so your 'formula' must be 'observation ~ treatment'")
expect_error(usktest(y_factor~Tratamento_unico,dados),"The variable 'y_factor' must be numeric\nThe variable 'Tratamento_unico' must have more than 1 type of treatment")
expect_warning(expect_error(usktest(y_factor~Tratamento_character,dados),"The variable 'y_factor' must be numeric"))
expect_error(expect_warning(usktest(y_factor~Tratamento_character,dados),"The variable 'Tratamento_character' has been changed to format 'factor'"))
#Gráfico
expect_equal(as.character(plot_usk(teste_005)$data$Group[6]),"d")
expect_equal(as.character(plot_usk(teste_099)$data$Group[6]),"f")
expect_equal(as.numeric(plotly_usk(teste_005)$x$data[[4]]$error_x$array),0.4331921,tolerance=0.01)
expect_equal(as.numeric(plotly_usk(teste_099)$x$data[[4]]$error_x$array),1.051205,tolerance=0.01)

#Alpha
expect_equal(usktest(y~Tratamento,dados,ANOVA = F)$Tratamento,resultado_005$Tratamento)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F)$Group,resultado_005$Group)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F)$Mean,resultado_005$Mean,tolerance=0.01)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F)$min,resultado_005$min,tolerance=0.01)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F)$max,resultado_005$max,tolerance=0.01)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,alpha=0.99)$Tratamento,resultado_099$Tratamento)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,alpha=0.99)$Group,resultado_099$Group)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,alpha=0.99)$Mean,resultado_099$Mean,tolerance=0.01)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,alpha=0.99)$min,resultado_099$min,tolerance=0.01)
expect_equal(usktest(y~Tratamento,dados,ANOVA = F,alpha=0.99)$max,resultado_099$max,tolerance=0.01)
#ANOVA
expect_equal(usktest(y~Tratamento,dados,ANOVA = T)[[3]][["Pr(>F)"]][1],9.573813e-11,tolerance=0.01)
#Variaveis com nome diferente
expect_equal(as.character(usktest(y~Tratamento,dados,ANOVA = T)[[1]]),"y")
expect_equal(as.character(usktest(y~Tratamento,dados,ANOVA = T)[[2]]),"Tratamento")
expect_equal(as.character(usktest(y~Tratamento,dados,ANOVA = F)[[1]]),"y")
expect_equal(as.character(usktest(y~Tratamento,dados,ANOVA = F)[[2]]),"Tratamento")
expect_equal(as.character(usktest(y_nome_diferente~Tratamento_nome_diferente,dados,ANOVA = T)[[1]]),"y_nome_diferente")
expect_equal(as.character(usktest(y_nome_diferente~Tratamento_nome_diferente,dados,ANOVA = T)[[2]]),"Tratamento_nome_diferente")
expect_equal(as.character(usktest(y_nome_diferente~Tratamento_nome_diferente,dados,ANOVA = F)[[1]]),"y_nome_diferente")
expect_equal(as.character(usktest(y_nome_diferente~Tratamento_nome_diferente,dados,ANOVA = F)[[2]]),"Tratamento_nome_diferente")
#Muitos tratamentos
set.seed(17)
taus=c(seq(from=2, to=400, by=4))
Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
erro<-rnorm(3*length(taus),0,1)
y<-2+taus+erro
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)
expect_equal(length(usktest(y~Tratamento,dados,ANOVA=F)[["Group"]]),100,tolerance=0)


a<-usktest(y~Tratamento,dados,ANOVA=F)
b<-usktest(y~Tratamento,dados,ANOVA=T)

