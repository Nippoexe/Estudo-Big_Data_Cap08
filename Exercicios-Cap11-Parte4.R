# Lista de Exercícios Parte 4 - Capítulo 11

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


# Definindo o Problema: OCR - Optical Character Recognition
# Seu modelo deve prever o caracter a partir do dataset fornecido. Use um modelo SVM

## Explorando e preparando os dados
setwd(input_path)
letters <- read.csv("letterdata.csv", stringsAsFactors = TRUE)
str(letters)


# Criando dados de treino e dados de teste
letters_treino <- letters[1:16000, ]
letters_teste  <- letters[16001:20000, ]

## Treinando o Modelo
#install.packages("kernlab")
library(kernlab)

# Criando o modelo com o kernel vanilladot
letter_classifier <- ksvm(letter ~ ., data = letters_treino, kernel = "vanilladot")
letter_classifier

letter_predictions <- predict(letter_classifier, letters_teste)

head(letter_predictions)

table(letter_predictions, letters_teste$letter)

agreement <- letter_predictions == letters_teste$letter

prop.table(table(agreement))
