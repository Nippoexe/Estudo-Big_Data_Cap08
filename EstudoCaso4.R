# Estatística na Prática 4 - Análise, Interpretação e Exercício com Teste do Qui-Quadrado

# Leia os manuais em pdf no Capítulo 11 do curso.

# Suposição do teste:
# As variáveis devem ser independentes!
projeto_path = "D:/FCDados/[11] - Machine Learning R/[03] - Projetos/"
input_path = "D:/FCDados/[11] - Machine Learning R/[01] - InputData/"
output_path = "D:/FCDados/[11] - Machine Learning R/[02] - OutputData/"
setwd(projeto_path)
getwd()

# Carregando o dataset
setwd(input_path)
df = read.csv("dados.csv")


# Visualizando os dados
View(df)

# Dimensões
dim(df)

# Separando x e y
x = df$Tipo_Imovel
unique(x)

y = df$Status_Imovel
unique(y)

# Tabela cruzada
table(x, y)
prop.table(table(x, y))*100

# Definindo as hipóteses:

# H0 = Não há relação entre x e y
# H1 = x e y estão relacionados

# Se o valor-p for menor que 0.05 rejeitamos a H0

# Teste do Qui-Quadrado
?chisq.test
chisq.test(table(x, y))

# Exercício:

# Se não considerarmos os imóveis do tipo Apartamento, há diferença no resultado do teste?
# Sim
?subset
?droplevels
df_2 <- subset(df, Tipo_Imovel != "Apartamento")
class(df_2)
View(df_2)


x = df_2$Tipo_Imovel
unique(x)

y = df_2$Status_Imovel
unique(y)

table(x, y)
prop.table(table(x, y))*100

chisq.test(table(x, y))

#Falhamos em rejeita H0
