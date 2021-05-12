########################################
###   MEDIDAS DE TENDÊNCIA CENTRAL   ###
########################################


#BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
if(!require(dplyr)) install.packages("dplyr") 

#CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

#MÉDIA

mean(enem2019_tratado$NOTA_MT)

#MEDIANA

median(enem2019_tratado$NOTA_MT)

#MODA

#Criando uma função
moda <- function(v) {
  valor_unico <- unique(v) # Busca o valor único para a coluna valor
  valor_unico[which.max(tabulate(match(v, valor_unico)))] #tabular (contabilizar quantas vezes o valor único aparece) e buscar o maior valor
}
#Obtendo a moda
resultado <- moda(enem2019_tratado$NOTA_MT)
print(resultado)

resultado <- moda(enem2019_tratado$NOTA_REDACAO)
print(resultado)


#HISTOGRAMA
#Análise matemática
hist(enem2019_tratado$NOTA_MT, probability=T, col="blue")
lines(density(enem2019_tratado$NOTA_MT) , col="red")


#Análise Redação
hist(enem2019_tratado$NOTA_REDACAO, probability=T, col="blue")
lines(density(enem2019_tratado$NOTA_REDACAO) , col="red")

mean(enem2019_tratado$NOTA_REDACAO)
median(enem2019_tratado$NOTA_REDACAO)
resultado <- moda(enem2019_tratado$NOTA_REDACAO)
print(resultado)








