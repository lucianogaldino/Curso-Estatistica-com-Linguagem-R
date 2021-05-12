#####################################################
####   DISTRIBUIÇÃO DE PROBABILIDADES DISCRETAS   ###
#####################################################

#BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
if(!require(dplyr)) install.packages("dplyr") 


#CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

#CRIANDO FUNÇÃO PROBABILIDADE
probab <- function(A, E) {
  resultado = A / E 
  print(resultado, digits = 3) 
}
#PROBABILIDADE DE RETIRAR UMA MULHER
mulheres_enem <- enem2019_tratado %>% filter(TP_SEXO=="F")
p <- probab(nrow(mulheres_enem), nrow(enem2019_tratado))

#DISTRIBUIÇÃO BINOMIAL
#dbinom = valor pontual   e   pbinom = faixa de valores

#PROBABILIDADE DE RETIRAR EXATAMENTE 4 MULHERES NUM TOTAL DE 10 AMOSTRAS
dbinom(4,10,p)   # Primeiro parametro:valor ou limite que se está acumulando
                 # Segundo: número de tentativas
                 # Terceiro: probabilidade de um sucesso.

#PROBABILIDADE DE RETIRAR PELO MENOS UMA MULHER NUM TOTAL DE 10 AMOSTRAS
p0 = 1 - dbinom(0,10,p)
p0

#PROBABILIDADE DE RETIRAR MAIS DO QUE 1 MULHER NUM TOTAL DE 10 AMOSTRAS
p1 = 1 - (dbinom(0,10,p)+dbinom(1,10,p))
p1

#PROBABILIDADE DE RETIRAR MAIS DO QUE 3 MULHERES NUM TOTAL DE 10 AMOSTRAS
p2 = 1 - (dbinom(0,10,p)+dbinom(1,10,p)+dbinom(2,10,p)+dbinom(3,10,p))
p2
p3 = 1 - pbinom(3, 10, p)
p3
#PROBABILIDADE DE RETIRAR MAIS DO QUE 8 MULHERES NUM TOTAL DE 10 AMOSTRAS
p4 = dbinom(9,10,p)+dbinom(10,10,p)
p4
p5 = pbinom(10, 10, p) - pbinom(8, 10, p)
p5





#DISTRIBUIÇÃO GEOMÉTRICA
#PROBABILIDADE DE RETIRAR 3 AMOSTRAS E NENHUMA SER MULHER
dgeom(3, p) #dgeom(x,p) x representa o número de fracassos e p a probabilidade





#DISTRIBUIÇÃO DE POISSON

# Num local de prova, 100 vestibulandos, normalmente, terminaram a prova em 2 horas (tempo mínimo).
# Probabilidade de exatamente 90 vestibulandos terminarem a prova em 2h.
dpois(90,100) #dpois(x, m) x é a quantidade de ocorrências EM ESTUDO e m é a taxa de ocorrências.








