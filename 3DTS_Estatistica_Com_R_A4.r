#-------------------------- Introdução ao R parte 4 --------------------------#
#--------------- Instalar os pacotes Tidyverse e Summarytools ----------------#

install.packages("summarytools")
install.packages("tidyverse")
install.packages("tibble")
install.packages("moments")

#--------- Carregar as bibliotecas tidyverse, ggplot2 e summarytools ---------#

library(tidyverse)
library(ggplot2)
library(summarytools)
library(moments)

#-----------------------------------   ---------------------------------------#

# Gerando números aleatórios
# rpois -> Gera números aleatórios com distribuição de poisso com a media = 1
x <- rpois(1000,1)
x

# Criando um histograma da variável 'x'
hist(x)

# usar a semente
set.seed(1)
x <- rpois(1000,1)
hist(x)


# importar arquivo Cartola_FC2018.xlsx em excel
library(readxl)
Cartola_FC2018 <- read_excel("X:/ESTUDOS/00_Acadêmico/MBA/06_STATISTICS WITH R - 36h/trab_aula04/Cartola_FC2018.xlsx")
View(Cartola_FC2018)

# verificar os tipos de variáveis
str(Cartola_FC2018)

# media de gols
media <- mean(Cartola_FC2018$gols)
media

# tabela de frequencia da variavel - número de gols
table(Cartola_FC2018$gols)
freq(Cartola_FC2018$gols)

# Gráfico da variável numero de gols
# biblioteca ggplot
g <- ggplot(Cartola_FC2018)
g + geom_bar(aes(gols)) +
    labs(title = "Distribuição do numero de gols. Brasileirão, 2018",
         x = "Número de gols", y = "Partidas")

# calcular a probabilidade usando media = 2.176316
# probabilidade de (X = 0) é: 0.1134587
prob_0 <- dpois(0,2.176316)
prob_0

# probabilidade de (X = 1) é: 0.2469221
prob_1 <- dpois(1,2.176316)
prob_1
# probabilidade de (X = 2) é: 0.2686902
prob_2 <- dpois(2,2.176316)
prob_2
# probabilidade de (X = 3) é: 0.1949183
prob_3 <- dpois(3,2.176316)
prob_3
# probabilidade de (X = 4) é: 0.1060509
prob_4 <- dpois(4,2.176316)
prob_4
# probabilidade de (X = 5) é: 0.04616007
prob_5 <- dpois(5,2.176316)
prob_5
# probabilidade de (X = 6) é: 0.01674315
prob_6 <- dpois(6,2.176316)
prob_6
# probabilidade de (X = 7) é: 0.005205484
prob_7 <- dpois(7,2.176316)
prob_7

#-----------------------------------------------------------------------------#
# Outra forma de calcular a probabilidade
prob <- dpois(c(0:7), 2.176316)
prob
#-----------------------------------------------------------------------------#

# 4)	Utilize a tabela Normal Padronizada e calcule a probabilidade:

# a)	P(Z < 0)  = 0.5
# b)	P(Z > 0) = 0.5
# Na tabela Z, procuramos o valor de Z que corresponde 
# à probabilidade acumulada menor que 0, e encontramos que é 0.5.

# c)	P(Z> 1.96) = 0.025
# d)	P(Z < -1.96) = 0.025
# Usando a propriedade de simetria da distribuição normal, a probabilidade de Z
# ser menor que -1.96 é igual à 
# probabilidade de Z ser maior que 1.96, que é 0.025.

# e)	P(-1.96 < Z < 1.96) = 0.95 
# Para calcular a probabilidade de Z estar entre -1.96 e 1.96, 
# podemos subtrair a probabilidade acumulada menor que -1.96 (0.025) 
# da probabilidade acumulada menor que 1.96 (0.975). 
# Isso resulta em 0.975 - 0.025 = 0.95.

#-----------------------------------------------------------------------------#

# Para calcular as probabilidades relacionadas ao tempo gasto no 
# exame de vestibular, que segue uma distribuição normal com média de 
# 120 minutos e desvio padrão de 15 minutos, você pode utilizar a tabela Z 
# ou a função de distribuição acumulada da distribuição normal no R. 
# Vou fornecer as respostas utilizando a função no R.

# a) Qual a probabilidade de um aluno terminar o exame antes de 130 minutos 
mean <- 120
sd <- 15
x <- 130

prob <- pnorm(x, mean, sd)
prob


# b) A probabilidade de um aluno terminar o exame antes de 100 minutos
x <- 100

prob <- pnorm(x, mean, sd)
prob

# c)	Qual deve ser o tempo de prova, de modo a permitir que 95% 
# dos vestibulandos terminem no prazo estipulado?
quantile <- qnorm(0.95, mean, sd)
quantile

#-----------------------------------------------------------------------------#
#----------------- Distribuição normal
# documentação distribuição normal
?dnorm

# Gerar números aleatórios com distribuição normal (media, desvio padrão)
x <- rnorm(1000, 0, 1)
hist(x)

set.seed(3)
y <- rnorm(100, 0, 2)
y
hist(y)

#-----------------------------------------------------------------------------#
#--------- IMportar o arquivo sal_çab1.xlsx
# Carregar o pacote "readxl" para ler o arquivo xlsx
library(readxl)

# Carregar a base de dados
sal_lab1 <- read_excel("X:/ESTUDOS/00_Acadêmico/MBA/06_STATISTICS WITH R - 36h/trab_aula04/sal_lab1.xlsx")

# Visualizar os dados
View(sal_lab1)
#-----------------------------------------------------------------------------#


# calcular a media e desvio padrão
# Media 9.136048
media_sal <- mean(sal_lab1$Ingestao_Sal)
media_sal

# Desvio padrão 2.316944
dp_sal <- sd(sal_lab1$Ingestao_Sal)
dp_sal

# Mediana 9.022889 
mediana <- quantile(sal_lab1$Ingestao_Sal, 0.50)
mediana

# Moda 
moda <- mode(sal_lab1$Ingestao_Sal)
moda
#-----------------------------------------------------------------------------#

# Grafico da variavel ingestao de sal
hist(sal_lab1$Ingestao_Sal)

# Assimetria 0.4319422
assimetria <- skewness(sal_lab1$Ingestao_Sal)
assimetria

# Calcular o z-score
sal_lab1$zscore = (sal_lab1$Ingestao_Sal - media_sal) / dp_sal
View(sal_lab1)
summary(sal_lab1$zscore)

# Grafico do qqplot (verificar se a distribuição)
qqnorm(sal_lab1$Ingestao_Sal)
qqline(sal_lab1$zscore)

# Histograma da variavel original e zscore
par(mfrow=c(1, 2))
par(mar=c(10, 4, 8, 2))
hist(sal_lab1$Ingestao_Sal, xlab = "Ingestão de sal", ylab = "Frequência",
     main = "histograma ingestão de sal")
hist(sal_lab1$zscore, xlab = "Z-score", ylab = "Frequência",
     main = "Histograma do Z-score de ingestão de sal")

# Histograma da variável original
hist(sal_lab1$Ingestao_Sal, main = "Histograma - Variável Original")

# Histograma da variável Zscore
hist(sal_lab1$zscore, main = "Histograma - Variável Zscore")

#-----------------------------------------------------------------------------#
