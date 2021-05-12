############################################################################
#   PREPARAÇÃO, ORGANIZAÇÃO E ESTRUTURAÇÃO DOS DADOS (PRÉ-PROCESSAMENTO)   #
############################################################################

# BAIXAR PACOTES
install.packages("dplyr") # Manipulação de Dados

# OU

# BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
if(!require(dplyr)) install.packages("dplyr") 

# CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019 <- read.csv('enem_sp_2019.csv')

View(enem2019)

# EXCLUIR UMA COLUNA
enem2019$NU_ANO <- NULL

# EXCLUIR VÁRIAS COLUNAS
excluir <- c("TP_ESTADO_CIVIL", "SG_UF_RESIDENCIA")
View (excluir)

enem2019 <- enem2019[  , !(names(enem2019) %in% excluir)]

View(names(enem2019))
# %in% verifica a intersecção em duas listas ou vetores.


#RENOMEAR UMA COLUNA
enem2019 <- rename(enem2019, NOTA_COMP1 = NU_NOTA_COMP1)
#RENOMEAR VÁRIAS COLUNAS
enem2019 <- rename(enem2019, NOTA_COMP2 = NU_NOTA_COMP2, NOTA_COMP3 = NU_NOTA_COMP3,
                   NOTA_COMP4 = NU_NOTA_COMP4,NOTA_COMP5 = NU_NOTA_COMP5,
                   NOTA_REDACAO = NU_NOTA_REDACAO, NOTA_CN = NU_NOTA_CN,
                   NOTA_CH = NU_NOTA_CH, NOTA_LC = NU_NOTA_LC, NOTA_MT = NU_NOTA_MT)













#VERIFICAÇÃO DA TIPAGEM DOS ATRIBUTOS (Variáveis)
# EXISTEM 6 TIPOS BÁSICOS:
# character (caracteres)
# integer (números inteiros)
# numeric (números reais)
# logical (falso ou verdadeiro)
# complex (números complexos)
# factor (fator: ordenar strings)
str(enem2019)
# OU
glimpse(enem2019)

#Transformando a variável Código escola em fator
enem2019$CO_ESCOLA <- as.factor(enem2019$CO_ESCOLA)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(enem2019, function(x) sum(is.na(x)))
sapply(enem2019, function(x) sum(is.nan(x)))












#TREINEIROS
treineiros <- enem2019 %>% filter(IN_TREINEIRO==1)# 93991 treineiros

#RETIRAR TREINEIROS
vestibulandos <- enem2019 %>% filter(IN_TREINEIRO==0)

# EXCLUIR UMA COLUNA
vestibulandos$IN_TREINEIRO <- NULL

#Exportando o arquivo treineiros
write.table(treineiros, file ="treineiros.csv", sep = ",")










#CRIANDO COLUNA PARA CLASSIFICAR AS PRESENÇAS
vestibulandos["presenca"] <- vestibulandos$TP_PRESENCA_CN + vestibulandos$TP_PRESENCA_CH +
                        vestibulandos$TP_PRESENCA_LC + vestibulandos$TP_PRESENCA_MT


falta_2dias <- vestibulandos %>% filter(presenca==0) # 185177 não compareceram nos dois dias
falta_1dia <- vestibulandos %>% filter(presenca==2) # 33726 não compareceram em um dos dias
desclas <- vestibulandos %>% filter(presenca==6) # 341 desclassificados em um dos dias
desclas_2vezes <- vestibulandos %>% filter(presenca==8) # nenhum desclassificado nos dois dias

#SELECIONANDO APENAS OS QUE COMPARECERAM NOS DOIS DIAS.
vestibulandos_presentes <- vestibulandos %>% filter(presenca==4)








# BAIXAR PACOTES PARA TRATAR VALORES MISSING
if(!require(tidyverse)) install.packages("tidyverse") 

library(tidyverse)

# Verificando valores missing
sapply(vestibulandos_presentes, function(x) sum(is.na(x)))

# EXCLUINDO VALORES MISSING
vestibulandos_presentes <- drop_na(vestibulandos_presentes, NOTA_MT)

sapply(vestibulandos_presentes, function(x) sum(is.na(x)))


# VERIFICANDO NOTAS ZEROS
nota_zero <- vestibulandos_presentes %>% filter(NOTA_REDACAO==0) #9181 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_COMP1==0) #9188 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_COMP2==0) #9181 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_COMP3==0) #9197 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_COMP4==0) #9194 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_COMP5==0) #73333 notas zeros

nota_zero <- vestibulandos_presentes %>% filter(NOTA_MT==0) #56 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_CH==0) #343 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_CN==0) #36 notas zeros
nota_zero <- vestibulandos_presentes %>% filter(NOTA_LC==0) #135 notas zeros

redacao_sem_prob <- vestibulandos_presentes %>% filter(TP_STATUS_REDACAO==1) # 490770 redações sem problemas

# EXCLUIR A COLUNA PRESENÇA
vestibulandos_presentes$presenca <- NULL

#EXPORTAR ARQUIVO TRATADO
write.table(vestibulandos_presentes, file ="enem2019_tratado.csv", sep = ",")


#####  SENSACIONAL  ###### 
