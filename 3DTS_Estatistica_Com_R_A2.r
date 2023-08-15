#-----------------Introdução ao R parte 2

# Matriz
m1 <- array(1:20, dim = c(4, 5))
m1

# selecionar a primeira linha da matriz
m1[1, ]
m1[, 2]
m1[2, 2]

#remover objetos
rm(list = ls())


# importando a base de dados
library(readr)
cadastro <- read_csv(
"C:/Users/Eliezer Junior/Git/MBA/3DTS-StatisticsWithR/Desafios/cadastro.csv"
       )
View(cadastro)

# tipos de variaveis
str(cadastro)

#medidas resumo
summary(cadastro)

# tipo de várioavel
str(cadastro)

# mudar o formato da variavel quantitativo para qualitativo
cadastro$NUM_CPF <- factor(cadastro$NUM_CPF)
cadastro$CEP <- factor(cadastro$CEP)

# verificar o tipo de variavel
str(cadastro)

# media, desvio e coeficente de variacao de renda
media_renda <- mean(cadastro$RENDA_PRESUMIDA)
media_renda

dp_renda <- sd(cadastro$RENDA_PRESUMIDA)
dp_renda

cv_renda <- dp_renda / media_renda
cv_renda

media_divida <- mean(cadastro$VALOR_DIVIDA)
media_divida

dp_divida <- sd(cadastro$VALOR_DIVIDA)
dp_divida

cv_divida <- dp_divida / media_divida
cv_divida

#histograma da variavel renda
hist(cadastro$RENDA_PRESUMIDA)

# grafico Box plot é usado para detectar outlier
boxplot(cadastro$RENDA_PRESUMIDA)

# criar a variavel outlier_renda

cadastro$outlier_renda <- ifelse(
cadastro$RENDA_PRESUMIDA > 5492, "ponto entremo",
       ifelse(cadastro$RENDA_PRESUMIDA > 4208, "outlier",
              ifelse(cadastro$RENDA_PRESUMIDA > 784, "não é outlier", "outlier")
                     )
)


# tabela de frequencia
table(cadastro$outlier_renda)

# media recortada

renda_sout <- cadastro[cadastro$outlier_renda == "não é outlier", ]

media_renda <- mean(renda_sout$RENDA_PRESUMIDA)
media_renda

dp_renda <- sd(renda_sout$RENDA_PRESUMIDA)
dp_renda

cv_renda <- dp_renda / media_renda
cv_renda
