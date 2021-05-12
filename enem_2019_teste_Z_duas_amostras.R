#####################################################
###    TESTE Z para duas amostras independentes   ###
#####################################################

if(!require(dplyr)) install.packages("dplyr")
if(!require(RVAideMemoire)) install.packages ("RVAideMemoire")
if(!require(BSDA)) install.packages("BSDA")

library(dplyr) 
library(RVAideMemoire) # Teste Shapiro por grupo
library(BSDA)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# Objetivo:
# Analisar a diferença entre as notas de Homens e mulheres de um colégio.

#Criando o dataframe de interesse
colegiox <- enem2019_tratado %>% filter(CO_ESCOLA=="35132287")


#NORMALIDADE
#Nível de significância (alfa) : 0,05
#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05

byf.shapiro(NOTA_CN ~ TP_SEXO, colegiox)
byf.shapiro(NOTA_CH ~ TP_SEXO, colegiox)
byf.shapiro(NOTA_LC ~ TP_SEXO, colegiox)
byf.shapiro(NOTA_MT ~ TP_SEXO, colegiox)
byf.shapiro(NOTA_REDACAO ~ TP_SEXO, colegiox)

# A nota de Redação não passou no teste da normalidade


colegiox_mulher <- colegiox %>% filter(TP_SEXO=="F")
colegiox_homem <- colegiox %>% filter(TP_SEXO=="M")




# TESTE Z PARA AMOSTRAS INDEPENDENTES
# Ho = NÃO HÁ DIFERENÇA ENTRE AS NOTAS : p > 0.05
# Ha = HÁ DIFERENÇA ENTRE AS NOTAS : p <= 0.05


#Comparação entre homens e mulheres das médias das notas do colégio X.

#TESTE-Z - CIÊNCIAS NATURAIS

sd(colegiox_mulher$NOTA_CN) #desvio padrão mulher
sd(colegiox_homem$NOTA_CN) #desvio padrão homem

#TESTE Z BICAUDAL
z.test(colegiox_mulher$NOTA_CN, sigma.x = 59.09334,
       colegiox_homem$NOTA_CN, sigma.y = 57.74716, alternative = "t")
?z.test




#TESTE-Z - CIÊNCIAS HUMANAS
sd(colegiox_mulher$NOTA_CH) #desvio padrão mulher
sd(colegiox_homem$NOTA_CH) #desvio padrão homem
mean(colegiox_mulher$NOTA_CH) - mean(colegiox_homem$NOTA_CH)
#Ho = média é igual a de São Paulo : p > 0.05
#Ha = média é diferente da de São Paulo : p <= 0.05

z.test(colegiox_mulher$NOTA_CH, sigma.x = 47.21453, 
       colegiox_homem$NOTA_CH, sigma.y = 52.88721)



#TESTE-Z - LINGUAGES E CÓDIGOS
sd(colegiox_mulher$NOTA_LC) #desvio padrão mulher
sd(colegiox_homem$NOTA_LC) #desvio padrão homem

z.test(colegiox_mulher$NOTA_LC, sigma.x = 32.86777, 
       colegiox_homem$NOTA_LC, sigma.y = 33.95025)



#TESTE-Z - MATEMÁTICA
sd(colegiox_mulher$NOTA_MT) #desvio padrão mulher
sd(colegiox_homem$NOTA_MT) #desvio padrão homem

z.test(colegiox_mulher$NOTA_MT, sigma.x = 80.81464, 
       colegiox_homem$NOTA_MT, sigma.y = 83.56487)



# CONCLUSÃO:
# PELO TESTE Z, NÃO PODEMOS AFIRMAR, NUM INTERVALO DE CONFIANÇA DE 95%,
# QUE AS NOTAS DE CIÊNCIAS HUMANAS SÃO DIFERENTES ENTRE HOMENS E MULHERES. JÁ AS
# NOTAS DE MATEMÁTICA, CIÊNCIAS NATURAIS E LINGUAGENS E CÓDIGOS, NUM INTERVALO DE 
# CONFIANÇA DE 95%, APONTAM QUE HÁ DIFERENÇA ENTRE AS NOTAS PARA HOMENS E MULHERES.


par(mfrow=c(1,4)) # Gráficos na mesma linha
boxplot(NOTA_CN ~ TP_SEXO, colegiox, ylab="Notas de Ciências Naturais", xlab="Gênero")
boxplot(NOTA_CH ~ TP_SEXO, colegiox, ylab="Notas de Ciências Humanas", xlab="Gênero")
boxplot(NOTA_MT ~ TP_SEXO, colegiox, ylab="Notas de Matemática", xlab="Gênero")
boxplot(NOTA_LC ~ TP_SEXO, colegiox, ylab="Notas de Linguages e códigos", xlab="Gênero")




