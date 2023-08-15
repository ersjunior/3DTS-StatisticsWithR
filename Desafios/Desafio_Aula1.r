# O desafio é importar uma base csv e mostra-la

# limpando as variáveis
rm(list=ls())

# importando a base de dados
library(readr)
cadastro <- read_csv("C:/Users/Eliezer Junior/Git/MBA/3DTS-StatisticsWithR/Desafios/cadastro.csv")
View(cadastro)
