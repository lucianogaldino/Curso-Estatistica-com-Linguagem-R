###########################################
###   CORRELAÇÃO DE SPEARMAN E KENDAL   ###
###########################################

# Testes de correlação não paramétricos

library(dplyr)                                
library(corrplot) # gráfico de correlação 
library(ggplot2)
library(ggpubr)


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

# ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# OBJETIVO:
# ANALISAR A CORRELAÇÃO LINEAR ENTRE AS NOTAS INDIVIDUAIS COM RELAÇÃO
# À NOTA FINAL DO COLÉGIO X.

# Criando o dataframe de interesse
colegiox=enem2019_tratado %>% filter(CO_ESCOLA=="35132287")

colegiox$NOTA_FINAL <- (colegiox$NOTA_CN + colegiox$NOTA_CH +
                          colegiox$NOTA_LC + colegiox$NOTA_MT +
                          colegiox$NOTA_REDACAO) / 5


# ANÁLISE DA CORRELAÇÃO LINEAR

# TESTE DE NORMALIDADE

# 1o) ANÁLISE GRÁFICA DA CORRELAÇÃO

plot(colegiox$NOTA_MT, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_LC, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_CH, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_CN, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_REDACAO, colegiox$NOTA_FINAL)

# 2o) NORMALIDADE
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(colegiox$NOTA_MT)
shapiro.test(colegiox$NOTA_LC)
shapiro.test(colegiox$NOTA_CH)
shapiro.test(colegiox$NOTA_CN)
shapiro.test(colegiox$NOTA_REDACAO) 
shapiro.test(colegiox$NOTA_FINAL)
# Nota de redação não passou no teste da normalidade


# CORRELAÇÃO DE SPEARMAN:
cor.test(colegiox$NOTA_REDACAO, colegiox$NOTA_FINAL, method = "spearman")


# CORRELAÇÃO DE KENDALL:
cor.test(colegiox$NOTA_REDACAO, colegiox$NOTA_FINAL, method = "kendall")

# Matrizes de correlação

matriz_corr <- cor(colegiox[25:26], method = "kendall")
View(matriz_corr)

corrplot(matriz_corr, method="color", 
         type="full", order="original", 
         addCoef.col = "black", # adiciona o coeficiente à matriz
         tl.col="black", tl.srt=45, # cor e rotação do nome das variáveis
)

# CONCLUSÃO:
# A Correlação de Spearman indicou uma correlação moderada e a correlação 
# de Kendall indicou uma correlação fraca entre a Nota da Redação e a Nota Final.



# REGRESSÃO LINEAR

# MODELO DE REGRESÃO LINEAR:
modelo_regressao_REDACAO <- lm(NOTA_REDACAO ~ NOTA_FINAL, colegiox)


# ANÁLISE DO MODELO DE REGRESSÃO
summary(modelo_regressao_REDACAO)
# Ho = COEFICIENTE = 0 : p > 0.05
# Ha = COEFICIENTE != 0 : p <= 0.05 (Modelo de regressão válido)


ggplot(data = colegiox, mapping = aes(x = NOTA_REDACAO, y = NOTA_FINAL)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 0) +
  theme_classic()


###  SENSACIONAL   ####
