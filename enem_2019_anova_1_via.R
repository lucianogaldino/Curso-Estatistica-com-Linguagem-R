############################
###   ANOVA de uma via   ###
############################

if(!require(dplyr)) install.packages("dplyr")
if(!require(rstatix)) install.packages("rstatix")
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools) # Teste Post-Hoc
library(dplyr)
library(rstatix)


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# OBJETIVO
# ANALISAR SE HÁ DIFERENÇA DE NOTA COM RELAÇÃO À RAÇA

colegiox <- enem2019_tratado %>% filter (CO_ESCOLA=="35132287")
str(colegiox)

colegiox$NOTA_FINAL <- (colegiox$NOTA_CN + colegiox$NOTA_CH +
                          colegiox$NOTA_LC + colegiox$NOTA_MT +
                          colegiox$NOTA_REDACAO) / 5
str(colegiox)



#ALTERANDO NÚMERO PELA RAÇA
colegiox$TP_COR_RACA[colegiox$TP_COR_RACA==0] <- "Não definido"
colegiox$TP_COR_RACA[colegiox$TP_COR_RACA==1] <- "Branca"
colegiox$TP_COR_RACA[colegiox$TP_COR_RACA==2] <- "Preta"
colegiox$TP_COR_RACA[colegiox$TP_COR_RACA==3] <- "Parda"
colegiox$TP_COR_RACA[colegiox$TP_COR_RACA==4] <- "Amarela"
colegiox$TP_COR_RACA[colegiox$TP_COR_RACA==5] <- "Indígena"

str(colegiox)

# Quantidades
branca = colegiox %>% filter(TP_COR_RACA=="Branca")
amarela = colegiox %>% filter(TP_COR_RACA=="Amarela")
preta = colegiox %>% filter(TP_COR_RACA=="Preta")
parda = colegiox %>% filter(TP_COR_RACA=="Parda")
indigena = colegiox %>% filter(TP_COR_RACA=="Indígena")
nd = colegiox %>% filter(TP_COR_RACA=="Não definido")

##### VERIFICAÇÃO DOS PRESSUPOSTOS #######
# Construção do modelo para obter os resíduos:
teste_anova <- aov(NOTA_FINAL ~ TP_COR_RACA, colegiox)

# Teste de normalidade para os resíduos:
shapiro.test(teste_anova$residuals)

# Verificação da presença de outliers entre os resíduos:
boxplot(teste_anova$residuals)


#######    TESTE ANOVA  ######
#Ho = média dos grupos são iguais: p > 0.05
#Ha = Há diferença entre pelo menos um dos grupos: p <= 0.05
teste_anova <- aov(NOTA_FINAL ~ TP_COR_RACA, colegiox)
summary(teste_anova)


# Análise post-hoc : Verifica quais grupos são diferentes, se houver
# Há mais de um tipo de teste, mais utilizados:

# Post-hoc Duncan (Mais flexível)
PostHocTest(teste_anova, method = "duncan", conf.level =0.95)
# Pos-hoc TukeyHSD (Mais regular)
PostHocTest(teste_anova, method = "hsd")
# Pos-hoc Bonferroni(Mais conservador)
PostHocTest(teste_anova, method = "bonf")

summary(colegiox$NOTA_FINAL)

#Conclusão:
# Não há diferença entre as médias conforme a Cor/Raça, segundo teste anova 1 via.
# F(3) = 1.261 e p_valor = 0.291



