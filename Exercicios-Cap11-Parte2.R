# Lista de Exercícios Parte 2 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
projeto_path = "D:/FCDados/[11] - Machine Learning R/[03] - Projetos/"
input_path = "D:/FCDados/[11] - Machine Learning R/[01] - InputData/"
output_path = "D:/FCDados/[11] - Machine Learning R/[02] - OutputData/"
setwd(projeto_path)
getwd()


# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Carregando o dataset
setwd(input_path)
df <- read.csv2('estudantes.csv')

# Explorando os dados
head(df)
summary(df)
str(df)
any(is.na(df))
View(df)
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)

#df$media = rowMeans(df[,c("G1", "G2", "G3")])

df$index <- runif(nrow(df))
trainset <- df[df$index <= 0.8,]
testset <- df[df$index >0.8,]

# Obter o índice 
trainColNum <- grep('index', names(trainset))

# Remover o índice dos datasets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]


modelo <- lm(G3 ~ ., data = trainset)
modelo

summary(modelo)

predict(modelo, testset)
plot(predict(modelo, testset))


