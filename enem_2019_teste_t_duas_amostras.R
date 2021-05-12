##########################################################
###    TESTE t Student (duas amostras independentes)   ###
##########################################################

if(!require(dplyr)) install.packages("dplyr")
if(!require(RVAideMemoire)) install.packages ("RVAideMemoire")
if(!require(car)) install.packages("car") 


library(dplyr) 
library(car) # Teste de homogeneidade (Levene)
library(RVAideMemoire) # Teste Shapiro por grupo

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# Objetivo:
# Analisar a diferença entre as notas de Homens e mulheres de um colégio.

# Criando o dataframe de interesse
colegioy <- enem2019_tratado %>% filter(CO_ESCOLA == "35151506")
str(colegioy)

#NORMALIDADE
#Nível de significância (alfa) : 0,05
#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05

byf.shapiro(NOTA_CN ~ TP_SEXO, colegioy)
byf.shapiro(NOTA_CH ~ TP_SEXO, colegioy)
byf.shapiro(NOTA_LC ~ TP_SEXO, colegioy)
byf.shapiro(NOTA_MT ~ TP_SEXO, colegioy)
byf.shapiro(NOTA_REDACAO ~ TP_SEXO, colegioy)

# A nota de Linguagens e códigos (LC) não passou no teste da normalidade.


# HOMOGENEIDADE DAS VARIÂNCIAS (HOMOCEDASTICIDADE)
# Variabilidade dos erros constante.
# Ho = variâncias homogêneas : p > 0.05
# Ha = variâncias não homogêneas : p <= 0.05

leveneTest(NOTA_CN ~ TP_SEXO, colegioy, center=mean)
leveneTest(NOTA_CH ~ TP_SEXO, colegioy, center=mean)
leveneTest(NOTA_MT ~ TP_SEXO, colegioy, center=mean)
leveneTest(NOTA_REDACAO ~ TP_SEXO, colegioy, center=mean)

# TESTE t PARA AMOSTRAS INDEPENDENTES
# Ho = NÃO HÁ DIFERENÇA ENTRE AS NOTAS : p > 0.05
# Ha = HÁ DIFERENÇA ENTRE AS NOTAS : p <= 0.05
# Default é bicaudal
# Para teste unicaudal deve colocar: alternative = "greater" ou alternative = "less"

t.test(NOTA_CN ~ TP_SEXO, colegioy, var.equal=TRUE)#variâncias homogêneas (var.equal=TRUE).
t.test(NOTA_CH ~ TP_SEXO, colegioy, var.equal=TRUE)
t.test(NOTA_MT ~ TP_SEXO, colegioy, var.equal=TRUE)
t.test(NOTA_REDACAO ~ TP_SEXO, colegioy, var.equal=TRUE)


# Pelo teste t, num intervalo de confiança de 95%, há diferença entre as notas de Ciências
# Naturais, Ciências Humanas e Matemática entre homens e mulheres. Já na Redação não se
# pode afirmar que há diferença entre as notas das mulheres e homens.


par(mfrow=c(1,4)) # Gráficos na mesma linha
boxplot(NOTA_CN ~ TP_SEXO, colegioy, ylab="Notas de Ciências Naturais", xlab="Gênero")
boxplot(NOTA_CH ~ TP_SEXO, colegioy, ylab="Notas de Ciências Humanas", xlab="Gênero")
boxplot(NOTA_MT ~ TP_SEXO, colegioy, ylab="Notas de Matemática", xlab="Gênero")
boxplot(NOTA_REDACAO ~ TP_SEXO, colegioy, ylab="Notas de Redação", xlab="Gênero")


####   SENSACIONAL   #####




