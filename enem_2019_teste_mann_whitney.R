###################################################
# TESTE DE MANN-WHITNEY (SOMA DE POSTOS WILCOXON) #
###################################################

if(!require(dplyr)) install.packages("dplyr")
if(!require(RVAideMemoire)) install.packages ("RVAideMemoire")
if(!require(rstatix)) install.packages("rstatix") 


library(rstatix)  
library(dplyr) 
library(RVAideMemoire) # Teste Shapiro por grupo

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

#Criando o dataframe de interesse
enem_bauru = enem2019_tratado[which(enem2019_tratado$NO_MUNICIPIO_RESIDENCIA=="Bauru"),]

# NORMALIDADE
# Ho = distribuição normal : p > 0.05
# Ha = distribuição não é normal : p <= 0.05

byf.shapiro(NOTA_CN ~ TP_SEXO, enem_bauru)
byf.shapiro(NOTA_CH ~ TP_SEXO, enem_bauru)
byf.shapiro(NOTA_LC ~ TP_SEXO, enem_bauru)
byf.shapiro(NOTA_MT ~ TP_SEXO, enem_bauru)
byf.shapiro(NOTA_REDACAO ~ TP_SEXO, enem_bauru)


# TESTE DE MANN-WHITNEY (SOMA DE POSTOS WILCOXON)
# Ho = mediana entre homens e mulheres são iguais : p > 0.05
# Ha = mediana entre homens e mulheres são diferentes : p <= 0.05
wilcox.test(NOTA_CN ~ TP_SEXO, data = enem_bauru)
wilcox.test(NOTA_CH ~ TP_SEXO, data = enem_bauru)
wilcox.test(NOTA_LC ~ TP_SEXO, data = enem_bauru)
wilcox.test(NOTA_MT ~ TP_SEXO, data = enem_bauru)
wilcox.test(NOTA_REDACAO ~ TP_SEXO, data = enem_bauru)


# ANÁLISE DESCRITIVA
enem_bauru %>% group_by(TP_SEXO) %>% 
  get_summary_stats(NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, NOTA_REDACAO, type = "median_iqr")


par(mfrow=c(1,5))
boxplot(NOTA_REDACAO ~ TP_SEXO, enem_bauru, ylab="Notas Redação", xlab="Gênero")
boxplot(NOTA_LC ~ TP_SEXO, enem_bauru, ylab="Notas Linguagens e Códigos", xlab="Gênero")
boxplot(NOTA_CN ~ TP_SEXO, enem_bauru, ylab="Notas Ciências Naturais", xlab="Gênero")
boxplot(NOTA_CH ~ TP_SEXO, enem_bauru, ylab="Notas Ciências Humanas", xlab="Gênero")
boxplot(NOTA_MT ~ TP_SEXO, enem_bauru, ylab="Notas Matemática", xlab="Gênero")

# CONCLUSÃO:
# PELO TESTE DE MANN-WHITNEY, SOMENTE AS MEDIANAS/DISTRIBUIÇÕES DAS NOTAS DE LINGUAGENS E CÓDIGOS
# ENTRE HOMENS E MULHERES SÃO ESTATISTICAMENTE IGUAIS, COM W=2079299 e p=0.3169.

