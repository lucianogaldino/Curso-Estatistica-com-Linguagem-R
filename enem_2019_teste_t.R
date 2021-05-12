######################################
###    TESTE t PARA UMA AMOSTRA    ###
######################################

#CONDIÇÕES: DISTRIBUIÇÃO NORMAL E NÚMERO DE AMOSTRAS MENOR QUE 30. 

if(!require(dplyr)) install.packages("dplyr")
if(!require(nortest)) install.packages("nortest")
if(!require(rstatix)) install.packages("rstatix") 
library(nortest)
library(dplyr)
library(rstatix)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

#Criando o dataframe de interesse
# colegioy = enem2019_tratado[which(enem2019_tratado$CO_ESCOLA=="35151506"),]
colegioy <- enem2019_tratado %>% filter(CO_ESCOLA == "35151506")
glimpse(colegioy)

#NORMALIDADE
#Nível de significância (alfa) : 0,05
#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05

#Ciências Humanas
#Shapiro-Wilk
shapiro.test(colegioy$NOTA_CH)
# Anderson-Darling
ad.test(colegioy$NOTA_CH)
#Lilliefors (Kolmogorov-Smirnov)
lillie.test(colegioy$NOTA_CH)
#Cramer-von Mises
cvm.test(colegioy$NOTA_CH)
#Histograma
hist(colegioy$NOTA_CH, probability=T, col="blue")
lines(density(colegioy$NOTA_CH) , col="red")
#QQplot
qqnorm(colegioy$NOTA_CH)
qqline(colegioy$NOTA_CH)

#TESTE-T
#Média das notas do colégio y, comparada a média do Estado de São Paulo.
#Ho = média é igual a de São Paulo : p > 0.05
#Ha = média é diferente da de São Paulo : p <= 0.05
summary(enem2019_tratado$NOTA_CH)

t.test(colegioy$NOTA_CH, mu = 529.6, alternative = "t")
?t.test

# INTERPRETAÇÃO:
# Intervalo de confiança de 95%
# Teste estatístico: t(29)= 7.1821
# p_valor = 6.616e-08
# Conclusão: A média do colégio y é diferente da média do Estado de São Paulo com 95% de nível de confiança.

# Observação: Média do colégio x
summary(colegioy$NOTA_CH)


#Gráfico Boxplot
par(mfrow=c(1,2))
boxplot(colegioy$NOTA_CH, ylab = "Nota de Ciências Humanas")
boxplot(enem2019_tratado$NOTA_CH, ylab = "Nota de Ciências Humanas")




