##############################
###   TESTE Qui-Quadrado   ###
##############################

if(!require(dplyr)) install.packages("dplyr")

library(dplyr) 


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# OBJETIVO
# ANALISAR SE HÁ RELAÇÃO DO GÊNERO COM A ESCOLHA DO IDIOMA 

proporcao <- as.data.frame(table(enem2019_tratado$TP_SEXO))
labels.prop<-c(round(((proporcao[,2])/sum(proporcao[,2]))*100,2))
labels.porcentagem <-paste(labels.prop, "%", sep=" ")
pie(x=c(proporcao[1,2],proporcao[2,2]), labels=labels.porcentagem, col=c("red", "blue"), main="Relação entre homens e mulheres")
legend("topright", pch=15, col=c("red","blue"), legend=c("Mulheres", "Homens"))


# Criação de uma tabela de contingência
tab <- table(enem2019_tratado$TP_SEXO, enem2019_tratado$TP_LINGUA)
tab

#Alterando as variáveis 0 e 1 por Inglês e Espanhol
enem2019_tratado$TP_LINGUA[enem2019_tratado$TP_LINGUA==0] <- "Inglês"
enem2019_tratado$TP_LINGUA[enem2019_tratado$TP_LINGUA==1] <- "Espanhol"

# Criação de uma tabela de contingência
tab <- table(enem2019_tratado$TP_SEXO, enem2019_tratado$TP_LINGUA)
tab

# TESTE QUI-QUADRADO
# Ho = NÃO há associação entre o gênero e a escolha do idioma : p > 0.05
# Ha = há associação entre o gênero e a escolha do idioma : p <= 0.05
teste_qui <- chisq.test(tab)
teste_qui


# Análise das frequências esperadas
teste_qui$expected


# CONCLUSÂO:
# Há uma dependência entre a escolha do idioma com o gênero.
# Pelo teste qui-quadrado:
# p_valor < 2,2 e-16
# X-squared(1) = 5210




