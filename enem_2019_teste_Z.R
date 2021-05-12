####################################
###   TESTE Z PARA UMA AMOSTRA   ###
####################################

# CONDIÇÕES: DISTRIBUIÇÃO NORMAL E DESVIO PADRÃO POPULACIONAL CONHECIDO. 

if(!require(dplyr)) install.packages("dplyr")
if(!require(nortest)) install.packages("nortest")
if(!require(BSDA)) install.packages("BSDA")
library(nortest)
library(dplyr)
library(BSDA) #teste Z

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

#Criando o dataframe de interesse
colegiox <- enem2019_tratado %>% filter(CO_ESCOLA=="35132287")

# Outra maneira que criar o data frame colegiox
# colegiox <- enem2019_tratado[which(enem2019_tratado$CO_ESCOLA=="35132287"),]

str(colegiox)
glimpse(colegiox)

#Verificando valores missing
sapply(colegiox, function(x) sum(is.na(x)))

# NORMALIDADE
# Nível de significância (alfa) : 0,05
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05

# Ciências Naturais
# Shapiro-Wilk
shapiro.test(colegiox$NOTA_CN)
# Anderson-Darling
ad.test(colegiox$NOTA_CN)
#Lilliefors (Kolmogorov-Smirnov)
lillie.test(colegiox$NOTA_CN)
# Cramer-von Mises
cvm.test(colegiox$NOTA_CN)
# Histograma
hist(colegiox$NOTA_CN, probability=T, col="blue")
lines(density(colegiox$NOTA_CN) , col="red")
# QQplot
qqnorm(colegiox$NOTA_CN)
qqline(colegiox$NOTA_CN)


# Média das notas do colégio X, comparada a média do Estado de São Paulo.
# TESTE-Z
summary(enem2019_tratado$NOTA_CN) #média do Estado de São Paulo
sd(enem2019_tratado$NOTA_CN) 
# Ho = média é igual a de São Paulo : p > 0.05
# Ha = média é diferente da de São Paulo : p <= 0.05


#TESTE Z BICAUDAL
z.test(colegiox$NOTA_CN, mu = 494.5, sigma.x = 78.62276, alternative = "t")
?z.test

# INTERPRETAÇÃO:
# Intervalo de confiança de 95%
# Teste estatístico: z = 16.483
# p_valor < 2.2e-16
# Conclusão: A média do colégio x é diferente da média do Estado de São Paulo com 95% de confiança.

# Observação: Média do colégio x
summary(colegiox$NOTA_CN)
summary(enem2019_tratado$NOTA_CN)


###   SENSACIONAL   #####
