###################################
###   TESTE DE KRUSKAL_WALLIS   ###
###################################

# Teste não paramétrico para mais de duas amostras (similar ao Anova)

library(dplyr)
library(rstatix)
library(DescTools)
library(nortest)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# OBJETIVO
# ANALISAR SE HÁ DIFERENÇA DE NOTA COM RELAÇÃO À RAÇA

enem2019_tratado$NOTA_FINAL <- (enem2019_tratado$NOTA_CN + enem2019_tratado$NOTA_CH +
                          enem2019_tratado$NOTA_LC + enem2019_tratado$NOTA_MT +
                          enem2019_tratado$NOTA_REDACAO) / 5
str(enem2019_tratado)

#ALTERANDO NÚMERO PELA RAÇA
enem2019_tratado$TP_COR_RACA[enem2019_tratado$TP_COR_RACA==0] <- "Não definido"
enem2019_tratado$TP_COR_RACA[enem2019_tratado$TP_COR_RACA==1] <- "Branca"
enem2019_tratado$TP_COR_RACA[enem2019_tratado$TP_COR_RACA==2] <- "Preta"
enem2019_tratado$TP_COR_RACA[enem2019_tratado$TP_COR_RACA==3] <- "Parda"
enem2019_tratado$TP_COR_RACA[enem2019_tratado$TP_COR_RACA==4] <- "Amarela"
enem2019_tratado$TP_COR_RACA[enem2019_tratado$TP_COR_RACA==5] <- "Indígena"

str(enem2019_tratado)

# Quantidades
branca = enem2019_tratado %>% filter(TP_COR_RACA=="Branca")
amarela = enem2019_tratado %>% filter(TP_COR_RACA=="Amarela")
preta = enem2019_tratado %>% filter(TP_COR_RACA=="Preta")
parda = enem2019_tratado %>% filter(TP_COR_RACA=="Parda")
indigena = enem2019_tratado %>% filter(TP_COR_RACA=="Indígena")
nd = enem2019_tratado %>% filter(TP_COR_RACA=="Não definido")

##### VERIFICAÇÃO DOS PRESSUPOSTOS #######
# NORMALIDADE
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
# Construção do modelo para obter os resíduos:
teste_anova <- aov(NOTA_FINAL ~ TP_COR_RACA, enem2019_tratado)
# Teste de normalidade para os resíduos:

#Lilliefors (Kolmogorov-Smirnov)
lillie.test(teste_anova$residuals)

# REPROVADO NO TESTE DE NORMALIDADE, TERÁ QUE SER REALIZADO UM TESTE NÃO PARAMÉTRICO.
# COMO SÃO MAIS DE DUAS VARIÁVEIS INDEPENDENTES, ENTÃO O TESTE SERÁ O KRUSKAL-WALLIS


# TESTE DE KRUSKAL-WALLIS
# Ho = mediana dos grupos são iguais: p > 0.05
# Ha = Há diferença das medianas pelo menos em um dos grupos: p <= 0.05
kruskal.test(NOTA_FINAL ~ TP_COR_RACA, data = enem2019_tratado)


# TESTE DE POS-HOC PARA VERIFICAR QUAIS SÃO AS DIFERENÇAS
# TESTE DUNN É O RECOMENDADO
dunn_test(NOTA_FINAL ~ TP_COR_RACA, data = enem2019_tratado, p.adjust.method = "bonferroni")

# Análise descritiva
enem2019_tratado %>% group_by(TP_COR_RACA) %>% get_summary_stats(NOTA_FINAL, type = "median_iqr")


# CONCLUSÃO:
# PELO TESTE DE KRUSKAL-WALLIS, AS NOTAS FINAIS NO ENEM, ESTATISTICAMENTE, 
# NÃO SÃO IGUAIS POR RAÇA. 


