# Lista de Exercícios Parte 3 - Capítulo 11

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


# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# Seu modelo deve prever a MEDV (Valor da Mediana de ocupação das casas). Utilize um modelo de rede neural!

# Carregando o pacote MASS
library(MASS)

# Importando os dados do dataset Boston
set.seed(101)
dados <- Boston
head(dados)
View(dados)

# Resumo dos dados
str(dados)
summary(dados)
any(is.na(dados))

# Carregando o pacote para Redes Neurais
#install.packages("neuralnet")
library(neuralnet)


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
norm_dados <- as.data.frame(lapply(dados, min_max_norm))
View(norm_dados)

norm_dados$index <- runif(nrow(norm_dados))
trainset <- norm_dados[norm_dados$index <= 0.8,]
testset <- norm_dados[norm_dados$index >0.8,]

# Obter o índice 
trainColNum <- grep('index', names(trainset))

# Remover o índice dos datasets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

modelo <- neuralnet(medv ~ ., trainset, hidden =c(5, 3), linear.output = FALSE)
modelo

plot(modelo)

x <- predict(modelo, testset, rep = 1, all.units = FALSE)
x

previsoes <- x * (max(dados$medv) - min(dados$medv)) + min(dados$medv)
teste_convert <- (testset$medv) * (max(dados$medv) - min(dados$medv)) + min(dados$medv)
teste_convert

error.df <- data.frame(teste_convert, previsoes)
head(error.df)
