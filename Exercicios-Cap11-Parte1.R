# Lista de Exercícios Parte 1 - Capítulo 11 

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

## Exercício 1 - Massa de dados aleatória

# Criando a massa de dados (apesar de aleatória, y possui 
# uma relação com os dados de x)
x <- seq(0, 100)
y <- 2 * x + 35

# Imprimindo as variáveis
x
y

# Gerando uma distribuição normal
y1 <- y + rnorm(101, 0, 50)
y1
hist(y1)

# Crie um plot do relacionamento de x e y1
plot(x, y1)

# Crie um modelo de regressão para as duas variáveis x e y1
modelo_x <- lm(y1 ~ x)
?lm

# Capture os coeficentes
a <- modelo_x$coefficients[1]
b <- modelo_x$coefficients[2]

# Fórmula de Regressão
y2 <- a + b*x

# Visualize a linha de regressão
?plot
lines(x, y2, lty = 3)

# Simulando outras possíveis linhas de regressão
y3 <- (y2[51]-50*(b-1))+(b-1)*x
y4 <- (y2[51]-50*(b+1))+(b+1)*x
y5 <- (y2[51]-50*(b+2))+(b+2)*x
lines(x,y3,lty=3)
lines(x,y4,lty=3)
lines(x,y5,lty=3)


## Exercício 2 - Pesquisa sobre idade e tempo de reação

# Criando os dados
Idade <- c(9,13,14,21,15,18,20,8,14,23,16,21,10,12,20,
           9,13,5,15,21)

Tempo <- c(17.87,13.75,12.72,6.98,11.01,10.48,10.19,19.11,
           12.72,0.45,10.67,1.59,14.91,14.14,9.40,16.23,
           12.74,20.64,12.34,6.44)


# Crie um Gráfico de Dispersão (ScatterPlot)
plot(Tempo, Idade)

# Teste com ggplot
#library("ggplot2")
#?ggplot
#data <- as.data.frame(Idade)
#data$Tempo <- Tempo
#data
#ggplot(data, mapping = aes(Tempo, Idade)) + geom_point()

# Crie um modelo de regressão
modelo_2 <- lm(Tempo ~ Idade)
modelo_2

# Calcule a reta de regressão
a_2 <- modelo_2$coefficients[1]
b_2 <- modelo_2$coefficients[2]

Idade_y <- a_2 + b_2*Tempo

# Crie o gráfico da reta
lines(Tempo, Idade_y, lty = 3)



# Exercício 3 - Relação entre altura e peso

# Criando os dados
alturas = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
pesos = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

plot(alturas, pesos, pch = 16, cex = 1.3, col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso Corporal (kg)", 
     xlab = "Altura (cm)")

# Crie o modelo de regressão
modelo_3 <- lm(pesos ~ alturas)

# Visualizando o modelo
modelo_3

# Gere a linha de regressão
a_3 <- modelo_3$coefficients[1]
b_3 <- modelo_3$coefficients[2]
y_3 <- a_3 + b_3*alturas  
y_3

#ggplot(mapping = aes(alturas, y_3)) + geom_smooth(method = "lm")
lines(alturas, y_3, lty = 3)

# Faça as previsões de pesos com base na nova lista de alturas
alturas2 = data.frame(c(179, 152, 134, 197, 131, 178, 185, 162, 155, 172))

predict(modelo_3, alturas2)

# Plot
plot(alturas, pesos, pch = 16, cex = 1.3, 
     col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso (kg)", 
     xlab = "Altura (cm)")

# Construindo a linha de regressão
abline(lm(pesos ~ alturas)) 

# Obtendo o tamanho de uma das amostras de dados
num <- length(alturas)
num

# Gerando um gráfico com os valores residuais
for (k in 1: num)  
  lines(c(alturas[k], alturas[k]), 
        c(pesos[k], pesos[k]))

# Gerando gráficos com a distribuição dos resíduos
par(mfrow = c(2,2))
plot(modelo_3)
