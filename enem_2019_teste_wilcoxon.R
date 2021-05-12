#############################
###   TESTE DE WILCOXON   ###
#############################

# Duas amostras dependentes não paramétricas (similar ao teste t pareado paramétrico)

if(!require(dplyr)) install.packages("dplyr")
if(!require(rstatix)) install.packages("rstatix") 

library(rstatix)  
library(dplyr) 


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Curso_estatistica_R")


#ABRIR ARQUIVO
enem2019_tratado <- read.csv('enem2019_tratado.csv', sep = ",")

# OBJETIVO
# ANALISAR AS DIFERENÇAS ENTRE MEDIANAS DAS NOTAS DOS COMPONENTES DA REDAÇÃO.


#Criando o dataframe de interesse
colegioy <- enem2019_tratado %>% filter(CO_ESCOLA=="35151506")

# NORMALIDADE
# A diferença entre as variáveis é que deve ser testada.
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

# As combinações reprovadas pelo teste de normalidade foram:
# NOTA_COMP1 com NOTA_COMP2;
# NOTA_COMP1 com NOTA_COMP3;
# NOTA_COMP1 com NOTA_COMP4;
# NOTA_COMP1 com NOTA_COMP5;
# NOTA_COMP2 com NOTA_COMP3;
# NOTA_COMP3 com NOTA_COMP4;
# NOTA_COMP4 com NOTA_COMP5;

# TESTE DE WILCOXON
# Ho = não há diferença nas medianas: p > 0.05
# Ha = existe diferença entre as medianas: p <= 0.05
wilcox.test(colegioy$NOTA_COMP1, colegioy$NOTA_COMP2, paired = TRUE)
wilcox.test(colegioy$NOTA_COMP1, colegioy$NOTA_COMP3, paired = TRUE)
wilcox.test(colegioy$NOTA_COMP1, colegioy$NOTA_COMP4, paired = TRUE)
wilcox.test(colegioy$NOTA_COMP1, colegioy$NOTA_COMP5, paired = TRUE)
wilcox.test(colegioy$NOTA_COMP2, colegioy$NOTA_COMP3, paired = TRUE)
wilcox.test(colegioy$NOTA_COMP3, colegioy$NOTA_COMP4, paired = TRUE)
wilcox.test(colegioy$NOTA_COMP4, colegioy$NOTA_COMP5, paired = TRUE)

# ANÁLISE PELO BOXPLOT
par(mfrow=c(1,5)) 
boxplot(colegioy$NOTA_COMP1, ylab="NOTAS", xlab="COMPONENTE 1")
boxplot(colegioy$NOTA_COMP2, ylab="NOTAS", xlab="COMPONENTE 2")
boxplot(colegioy$NOTA_COMP3, ylab="NOTAS", xlab="COMPONENTE 3")
boxplot(colegioy$NOTA_COMP4, ylab="NOTAS", xlab="COMPONENTE 4")
boxplot(colegioy$NOTA_COMP5, ylab="NOTAS", xlab="COMPONENTE 5")

summary(colegioy[c('NOTA_COMP1','NOTA_COMP2', 'NOTA_COMP3', 'NOTA_COMP4', 'NOTA_COMP5')])


# CONCLUSÃO:
# PELO TESTE DE WILCOXON, NÃO HÁ DIFERENÇA ENTRE AS MEDIANAS DAS NOTAS DOS COMPONENTES 
# DA REDAÇÃO NO COLÉGIO Y. 



#####  SENSACIONAL  #####




