#######################################
###   Distribuição de frequências   ###
#######################################

#BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
if(!require(dplyr)) install.packages("dplyr") 
if(!require(sampling)) install.packages("sampling")
if(!require(TeachingSampling)) install.packages("TeachingSampling")


#CARREGAR PACOTES
library(dplyr)
library(sampling)
library(TeachingSampling)


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")
getwd()

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")


#SOROCABA
enem_sorocaba <- enem2019_tratado %>% filter(NO_MUNICIPIO_RESIDENCIA=="Sorocaba")

# Tabela de Frequências Absolutas
freq_abs <- table(enem_sorocaba$NU_IDADE) 
View(freq_abs)

# Tabela de Frequências Relativas
freq_rel <- prop.table(freq_abs) 
View(freq_rel)

# Porcentagem da frequência relativa
p_freq_rel <- 100 * prop.table(freq_rel) 
View(p_freq_rel)

# Criar uma linha com o total
freq_abs <- c(freq_abs, sum(freq_abs)) 
View(freq_abs)
names(freq_abs)[54] <- "Total"
View(freq_abs)

# Juntando a frequência relativa e a frequência percentual com suas respectivas somas.
freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel))

# Tabela final com todos os valores
tabela_final <- cbind(freq_abs, 
                      freq_rel = round(freq_rel, digits = 5), 
                      p_freq_rel = round(p_freq_rel, digits = 2))
View(tabela_final)


#CONSTRUINDO CLASSES DE FREQUÊNCIAS
intervalo_classes <- seq(10,75,5)
View(intervalo_classes)
tabela_classes <- table(cut(enem_sorocaba$NU_IDADE, breaks=intervalo_classes, right=FALSE))
View(tabela_classes)











# GRÁFICOS DE FREQUÊNCIA

# Histograma
hist(enem_sorocaba$NU_IDADE, col = "red")

# Polígono de frequência
plot(tabela_classes,type='o')
?plot

# GRÁFICO DE OGIVA

# Frequência Acumulada
freq_rel_classes <- prop.table(table(cut(enem_sorocaba$NU_IDADE,
                             breaks = c(intervalo_classes))))
View(freq_rel_classes)
freq_acum <- cumsum(tabela_classes)[seq_along(intervalo_classes)]
View(freq_acum)

# GRÁFICO
plot(intervalo_classes, freq_acum, type='o')





