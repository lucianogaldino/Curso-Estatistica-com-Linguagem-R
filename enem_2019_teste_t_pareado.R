#############################
###    TESTE t PAREADO    ###
#############################

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr) 



# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# OBJETIVO
# ANALISAR AS DIFERENÇAS ENTRE MÉDIAS DAS NOTAS DOS COMPONENTES DA REDAÇÃO.

#Criando o dataframe de interesse
colegioy <- enem2019_tratado %>% filter(CO_ESCOLA=="35151506")


# NORMALIDADE

# A diferença entre as variáveis é que deve ser normal.
# Nível de significância (alfa) : 0,05
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05


colegioy$DiferencaNotas <- colegioy$NOTA_COMP1 - colegioy$NOTA_COMP2
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP1 - colegioy$NOTA_COMP3
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP1 - colegioy$NOTA_COMP4
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP1 - colegioy$NOTA_COMP5
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP2 - colegioy$NOTA_COMP3
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP2 - colegioy$NOTA_COMP4
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP2 - colegioy$NOTA_COMP5
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP3 - colegioy$NOTA_COMP4
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP3 - colegioy$NOTA_COMP5
shapiro.test(colegioy$DiferencaNotas)
colegioy$DiferencaNotas <- colegioy$NOTA_COMP4 - colegioy$NOTA_COMP5
shapiro.test(colegioy$DiferencaNotas)
# As combinações aprovadas pelo teste de normalidade foram:
# NOTA_COMP2 com NOTA_COMP4;
# NOTA_COMP2 com NOTA_COMP5;
# NOTA_COMP3 com NOTA_COMP5;



# TESTE T PAREADO
# Ho = NÃO HÁ DIFERENÇA ENTRE AS MÉDIAS (DIFERENÇA MÉDIA = 0) : p > 0.05
# Ha = HÁ DIFERENÇA ENTRE AS MÉDIAS (DIFERENÇA MÉDIA != 0) : p <= 0.05


t.test(colegioy$NOTA_COMP2, colegioy$NOTA_COMP4, paired = TRUE) #paired é pareado
# CONCLUSÃO:
# NÃO HÁ DIFERENÇA ENTRE AS MÉDIAS, SENDO INTERVALO DE CONFIANÇA DE 95%,
# t (29) = - 0.72351
# p_valor = 0,4752
# Média das diferenças = - 3,33



t.test(colegioy$NOTA_COMP2, colegioy$NOTA_COMP5, paired = TRUE)
# CONCLUSÃO:
# NÃO HÁ DIFERENÇA ENTRE AS MÉDIAS, SENDO INTERVALO DE CONFIANÇA DE 95%,
# t (29) = - 0.22003
# p_valor = 0,8274
# Média das diferenças = -1,33




t.test(colegioy$NOTA_COMP3, colegioy$NOTA_COMP5, paired = TRUE)
# CONCLUSÃO:
# NÃO HÁ DIFERENÇA ENTRE AS MÉDIAS, SENDO INTERVALO DE CONFIANÇA DE 95%,
# t (29) = - 0.47186
# p_valor = 0,6406
# Média das diferenças = -2,67



# ANÁLISE PELO BOXPLOT
par(mfrow=c(1,2)) 
boxplot(colegioy$NOTA_COMP2, ylab="NOTAS", xlab="COMPONENTE 2")
boxplot(colegioy$NOTA_COMP5, ylab="NOTAS", xlab="COMPONENTE 4")

par(mfrow=c(1,2)) 
boxplot(colegioy$NOTA_COMP2, ylab="NOTAS", xlab="COMPONENTE 2")
boxplot(colegioy$NOTA_COMP5, ylab="NOTAS", xlab="COMPONENTE 5")

par(mfrow=c(1,2)) 
boxplot(colegioy$NOTA_COMP3, ylab="NOTAS", xlab="COMPONENTE 3")
boxplot(colegioy$NOTA_COMP5, ylab="NOTAS", xlab="COMPONENTE 5")

# MEDIDAS DE CENTRALIDADE E POSIÇÃO
summary(colegioy[c('NOTA_COMP1','NOTA_COMP2', 'NOTA_COMP3', 'NOTA_COMP4', 'NOTA_COMP5')])


