#------------------------- Estatistica com R parte 5 -------------------------#
### Importando as bibliotécas necessárias
library(tidyverse)
library(summarytools)
library(gmodels)

?sample

## Importar a base de cadastro
library(readr)
df <- read_csv("X:/ESTUDOS/00_Acadêmico/MBA/06_STATISTICS WITH R - 36h/Aula 5/trab_Aula5/Arq_trab5.csv")

### Conhecendo a população
### Tabela de frequência da variável Sexo na população
freq(df$Sexo)

### Calcular a média da idade da população
summary(df$idade)

### Calcular a média de anos de estudo da população
summary(df$Anos_estudo)

### Sortear uma amostra aleatória de tamanho 40
set.seed(7)
amostra <- df[sample(1:nrow(df),40, replace = FALSE),]

### Conhecendo a amostra criada
### Tabela de frequência da variável Sexo na base de amostra
freq(amostra$Sexo)

### Calculando a idade média da base de amostra
summary(amostra$idade)

### Calcular a media de anos de estudo da base de amostra
summary(amostra$Anos_estudo)

### Caclular a proporção de clientes com telefone móvel na amostra
nobs <- nrow(amostra)
tem_telefone <- ifelse(amostra$Telefone_movel == 2, 1, 0)
soma <- sum(tem_telefone)
proporcao <- soma/nobs
proporcao

### Calculando o erro pardão
ep <- sqrt(proporcao*(1-proporcao)/nobs)
ep

### Calculando o intervalo de confiança
limite_inferior <- proporcao - 1.96*ep
limite_superior <- proporcao + 1.96*ep

limite_inferior
limite_superior

### Interpretação do Intervalo de confiança de 95%
# A Proporcao de clientes com telefone movel na populacao
# esta entre 77.2% e 97.7%
  