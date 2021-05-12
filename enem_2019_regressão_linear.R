####################################
###   REGRESSÃO LINEAR SIMPLES   ###
####################################


if(!require(dplyr)) install.packages("dplyr")
if(!require(corrplot)) install.packages("corrplot")
if(!require(car)) install.packages("car")
if(!require(ggplot2)) install.packages("ggplot2") #gráfico com ajuste de reta
if(!require(ggpubr)) install.packages("ggpubr") #equação da reta no gráfico
library(dplyr)                                
library(corrplot) # gráfico de correlação 
library(car) # Homocedasticidade (Levene)
library(ggplot2)
library(ggpubr)


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")

#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

#Criando o dataframe de interesse
colegiox <- enem2019_tratado %>% filter (CO_ESCOLA=="35132287")


colegiox$NOTA_FINAL <- (colegiox$NOTA_CN + colegiox$NOTA_CH +
                          colegiox$NOTA_LC + colegiox$NOTA_MT +
                          colegiox$NOTA_REDACAO) / 5


# ANÁLISE DA CORRELAÇÃO LINEAR

# DADOS NUMÉRICOS NORMALIZADOS: CORRELAÇÃO DE PEARSON

# 1o) ANÁLISE GRÁFICA DA CORRELAÇÃO

plot(colegiox$NOTA_MT, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_LC, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_CH, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_CN, colegiox$NOTA_FINAL)
plot(colegiox$NOTA_REDACAO, colegiox$NOTA_FINAL)

# 2o) NORMALIDADE
#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05
shapiro.test(colegiox$NOTA_MT)
shapiro.test(colegiox$NOTA_LC)
shapiro.test(colegiox$NOTA_CH)
shapiro.test(colegiox$NOTA_CN)
shapiro.test(colegiox$NOTA_REDACAO) 
shapiro.test(colegiox$NOTA_FINAL)
# Nota de redação não passou no teste da normalidade

# 3o) ANÁLISE DE OUTLIERS
boxplot(colegiox$NOTA_MT)
boxplot(colegiox$NOTA_LC)
boxplot(colegiox$NOTA_CN)
boxplot(colegiox$NOTA_CH)
boxplot(colegiox$NOTA_FINAL)

# Gráfico para verificação dos resíduos (valor previsto - valor esperado)

# MODELO DE REGRESÃO LINEAR:
modelo_regressao_MT <- lm(NOTA_MT ~ NOTA_FINAL, colegiox)
## Análise gráfica:
plot(modelo_regressao_MT, , which=c(1, 2)) # vai plotar os gráficos 1 e 2 da função.

modelo_regressao_LC <- lm(NOTA_LC ~ NOTA_FINAL, colegiox)
## Análise gráfica:
plot(modelo_regressao_LC, which=c(1, 2))
# Outlier interferindo

modelo_regressao_CN <- lm(NOTA_CN ~ NOTA_FINAL, colegiox)
# Análise gráfica:
plot(modelo_regressao_CN, which=c(1, 2))

modelo_regressao_CH <- lm(NOTA_CH ~ NOTA_FINAL, colegiox)
# Análise gráfica:
plot(modelo_regressao_CH, which=c(1, 2))



# Correlação Linear de Pearson:
# Ho = não há corrrelação linear: p > 0,05
# Ha = existe correlação linear: p <= 0,05
cor.test(colegiox$NOTA_MT, colegiox$NOTA_FINAL, method = "pearson")
cor.test(colegiox$NOTA_LC, colegiox$NOTA_FINAL, method = "pearson")
cor.test(colegiox$NOTA_CN, colegiox$NOTA_FINAL, method = "pearson")
cor.test(colegiox$NOTA_CH, colegiox$NOTA_FINAL, method = "pearson")

# Matrizes de correlação
#Alterando posição da coluna NOTA_FINAL
colegiox <- colegiox %>% relocate(NOTA_FINAL, .after = NOTA_MT)

matriz_corr <- cor(colegiox[14:18], method = "pearson")
View(matriz_corr)

corrplot(matriz_corr, method = "color")

corrplot(matriz_corr, method="color", 
         type="full", order="original", 
         addCoef.col = "black", # adiciona o coeficiente à matriz
         tl.col="black", tl.srt=45, # cor e rotação do nome das variáveis
)






# REGRESSÃO LINEAR


# MODELO DE REGRESÃO LINEAR:
modelo_regressao_MT <- lm(NOTA_MT ~ NOTA_FINAL, colegiox)

modelo_regressao_LC <- lm(NOTA_LC ~ NOTA_FINAL, colegiox)

modelo_regressao_CN <- lm(NOTA_CN ~ NOTA_FINAL, colegiox)

modelo_regressao_CH <- lm(NOTA_CH ~ NOTA_FINAL, colegiox)

# NORMALIDADE DOS RESÍDUOS
#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05
shapiro.test(modelo_regressao_MT$residuals)
shapiro.test(modelo_regressao_LC$residuals)
shapiro.test(modelo_regressao_CN$residuals)
shapiro.test(modelo_regressao_CH$residuals)

# Matemática e Ciências Naturais: reprovadas no teste da normalidade dos resíduos.

# ANÁLISE DO MODELO DE REGRESSÃO
summary(modelo_regressao_MT)
# Ho = COEFICIENTE = 0 : p > 0.05
# Ha = COEFICIENTE != 0 : p <= 0.05 (Modelo de regressão válido)


ggplot(data = colegiox, mapping = aes(x = NOTA_MT, y = NOTA_FINAL)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 0) +
  theme_classic()




summary(modelo_regressao_LC)
# Ho = COEFICIENTE = 0 : p > 0.05
# Ha = COEFICIENTE != 0 : p <= 0.05 (Modelo de regressão válido)


ggplot(data = colegiox, mapping = aes(x = NOTA_LC, y = NOTA_FINAL)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 500, label.y = 500) +
  theme_classic()



summary(modelo_regressao_CN)
# Ho = COEFICIENTE = 0 : p > 0.05
# Ha = COEFICIENTE != 0 : p <= 0.05 (Modelo de regressão válido)


ggplot(data = colegiox, mapping = aes(x = NOTA_CN, y = NOTA_FINAL)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 0) +
  theme_classic()



summary(modelo_regressao_CH)
# Ho = COEFICIENTE = 0 : p > 0.05
# Ha = COEFICIENTE != 0 : p <= 0.05 (Modelo de regressão válido)


ggplot(data = colegiox, mapping = aes(x = NOTA_CH, y = NOTA_FINAL)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 0) +
  theme_classic()



####   SENSACIONAL   ####



