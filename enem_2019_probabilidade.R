##############################
####     PROBABILIDADE     ###
##############################

# BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
if(!require(dplyr)) install.packages("dplyr") 


# CARREGAR PACOTES
library(dplyr)


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

# ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")


enem_guarulhos <- enem2019_tratado %>% filter(NO_MUNICIPIO_RESIDENCIA=="Guarulhos")
nrow(enem_guarulhos)
enem_campinas <- enem2019_tratado %>% filter(NO_MUNICIPIO_RESIDENCIA=="Campinas")
nrow(enem_campinas)



# CRIANDO FUNÇÃO PROBABILIDADE
probab <- function(A, E) {
        resultado = (A / E)*100 
        print(resultado, digits = 3) 
}
# PROBABILIDADE DE RETIRAR UM VESTIBULANDO DE GUARULHOS
probab(nrow(enem_guarulhos), nrow(enem2019_tratado))

# PROBABILIDADE DE RETIRAR UM VESTIBULANDO DE GUARULHOS
probab(nrow(enem_campinas), nrow(enem2019_tratado))




# CRIANDO FUNÇÃO PROBABILIDADE DE NÃO OCORRER UM EVENTO
probab_negativa <- function(A, E) {
  resultado = (1 - (A / E))*100 
  print(resultado, digits = 3) 
}
# PROBABILIDADE DE NÃO RETIRAR UM VESTIBULANDO DE GUARULHOS E NEM DE CAMPINAS
probab_negativa((nrow(enem_guarulhos)+nrow(enem_campinas)), nrow(enem2019_tratado))





#CRIANDO FUNÇÃO PROBABILIDADE DA UNIÃO MUTUAMENTE EXCLUSIVOS (AUB)
probab_uniao <- function(A, B, E) {
  resultado = (A/E + B/E)*100
  print(resultado, digits = 3) 
}
#PROBABILIDADE DE RETIRAR UM VESTIBULANDO DE GUARULHOS OU DE CAMPINAS
probab_uniao(nrow(enem_guarulhos), nrow(enem_campinas), nrow(enem2019_tratado))





#CRIANDO FUNÇÃO PROBABILIDADE DA INTERSECÇÃO DE DOIS EVENTOS
probab_inter <- function(A, B, E) {
  resultado = (A/E * B/E)*100
  print(resultado, digits = 3) 
}
#PROBABILIDADE DE RETIRAR UM VESTIBULANDO DE GUARULHOS E DE CAMPINAS (com reposição)
probab_inter(nrow(enem_guarulhos), nrow(enem_campinas), nrow(enem2019_tratado))





#CRIANDO FUNÇÃO PROBABILIDADE CONDICIONAL
probab_cond <- function(AB, B) {
  resultado = (AB / B)*100
  print(resultado, digits = 3) 
}
#PROBABILIDADE DE RETIRAR UMA MULHER PARDA
mulher <- enem2019_tratado %>% filter(TP_SEXO=="F")
mulher_parda <- mulher %>% filter(TP_COR_RACA == 3)

probab_cond(nrow(mulher_parda), nrow(mulher))


#####   SENSACIONAL  #####

