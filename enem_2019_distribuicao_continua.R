##########################################################
####    DISTRIBUIÇÃO CONTÍNUA -TESTES DE NORMALIDADE   ###
##########################################################


#TESTES DE NORMALIDADE

# Existem 4 testes de normalidade principais (numéricos) e dois testes gráficos:
# Shapiro-Wilk (limite de 5000 amostras)
# Anderson-Darling
# Kolmogorov_Smirnov
# Cramer-Von Mises
# Histograma
# QQplot

# NÍVEL DE SIGNIFICÂNCIA DE 0,05 ou 5% (MAIS UTILIZADO):
# QUANDO p > 0,05 (distribuição normal).


if(!require(dplyr)) install.packages("dplyr")
if(!require(nortest)) install.packages("nortest")
library(nortest)
library(dplyr)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")


#CIÊNCIAS NATURAIS

#Histograma
hist(enem2019_tratado$NOTA_CN, probability=T, col="blue")
lines(density(enem2019_tratado$NOTA_CN) , col="red")

# QQPLOT (GRÁFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(enem2019_tratado$NOTA_CN)
qqline(enem2019_tratado$NOTA_CN)

# Shapiro-Wilk
shapiro.test(enem2019_tratado$NOTA_CN)

# Anderson-Darling
ad.test(enem2019_tratado$NOTA_CN)

# Kolmogorov_Smirnov
ks.test(enem2019_tratado$NOTA_CN, pnorm)
lillie.test(enem2019_tratado$NOTA_CN)

#Cramer-Von Mises
cvm.test(enem2019_tratado$NOTA_CN) 




#ANÁLISE NA CIDADE DE FRANCA
enem_franca <- enem2019_tratado %>% filter(NO_MUNICIPIO_RESIDENCIA =="Franca")


#CIÊNCIAS NATURAIS

#Histograma
hist(enem_franca$NOTA_CN, probability=T, col="blue")
lines(density(enem_franca$NOTA_CN) , col="red")

# QQPLOT (GRÁFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(enem_franca$NOTA_CN)
qqline(enem_franca$NOTA_CN)

# Shapiro-Wilk
shapiro.test(enem_franca$NOTA_CN)

# Anderson-Darling
ad.test(enem_franca$NOTA_CN)

# Kolmogorov_Smirnov
lillie.test(enem_franca$NOTA_CN)

#Cramer-Von Mises
cvm.test(enem_franca$NOTA_CN) 


