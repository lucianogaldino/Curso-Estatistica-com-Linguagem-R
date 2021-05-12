###########################
##  ABERTURA DO ARQUIVO  ##
###########################


# Instalação dos pacotes
# Se não estiver instalado
#--------------------
if(!require(data.table)){install.packages('data.table')}
if(!require(dplyr)) install.packages("dplyr")

# Carregamento do pacote
library (data.table)
library(dplyr)

# Alocação de memória
#---------------
memory.limit(24576)
#------------------

# FORNECENDO DIRETÓRIO DO ARQUIVO
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

# Carga dos microdados

ENEM_2019 <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = " ", 
                               showProgress = TRUE)

# Filtrando apenas os dados do Estado de São Paulo
enem_sp_2019 <- ENEM_2019 %>% filter(SG_UF_RESIDENCIA =="SP")

# Selecionando as colunas de interesse
enem_sp_2019 <- select(enem_sp_2019, NU_INSCRICAO, NU_ANO, NO_MUNICIPIO_RESIDENCIA,
                       SG_UF_RESIDENCIA, NU_IDADE, TP_SEXO, TP_ESTADO_CIVIL, TP_COR_RACA,
                       TP_NACIONALIDADE, TP_ESCOLA, TP_ENSINO, IN_TREINEIRO, CO_ESCOLA,
                       TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT,
                       NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, TP_LINGUA,
                       TP_STATUS_REDACAO, NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3,
                       NU_NOTA_COMP4, NU_NOTA_COMP5, NU_NOTA_REDACAO)
View(enem_sp_2019)

#EXPORTAR ARQUIVO
write.table(enem_sp_2019, file ="enem_sp_2019.csv", sep = ",")




