#-----------------Introdução ao R

# calculadora

2+2
5-2
5/2
5*2
5^2
5^3

sqrt(5)

# vetor x = 10.4, 5.6, 3.1, 6.4, 21.7

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)

# divisão

1/x

# multiplicação

5*x

# somar

a <- sum(x)
a

# operadores matemáticos: +, -, /, *, ^, sqrt(Raiz Quadrada)

y <- 2*x^2 + 4*x + 1;y

# comprimento do vetor y
length(y)
length(x)

z <- c(x,y)
z

# removendo um objeto
rm(a)

# removendo todos os objetos
rm(list=ls())

# operadores logicos: >, >=, <, <=, &, |, !=(diferente), ==, !(negação)

10 > 15
10 < 15
10 == 15
10 != 15

# sequencia de valores

x <- c(1:10)
x

# média aritimetica
media_x <- mean(x)
media_x

# desvio padrao
dp_x <- sd(x)
dp_x

# criar um vetor com missing (NA)

z <- c(1:10,NA)
z

media_z <- mean(z, na.rm = TRUE)
media_z

# desvio padrao (sd)
dp_z <- sd(z, na.rm = TRUE)
dp_z

# quartis: quartil1, quartil2, quartil3
quartil1 <- quantile(x,probs = 0.25)
quartil1

quartil2 <- quantile(x,probs = 0.50)
quartil2

quartil3 <- quantile(x,probs = 0.75)
quartil3

# maximo e minimo
xmin <- min(x)
xmin

xmax <- max(x)
xmax

# criando uma função que calcula a média

media <- function(vetor) {
  soma <- sum(vetor)
  nobs <- length(vetor)
  md <- soma/nobs
}

b <- c(2*1:20)
b

# criando uma função que calcula o desvio padrao da populacao
func_desvio_padrao <- function(vetor) {
  soma <- sum(vetor)
  nobs <- length(vetor)
  media <- soma/nobs
  diferenca <- (vetor - media)^2
  somadif <- sum(diferenca)
  variancia <- somadif/nobs
  desvpad <- sqrt(variancia)
  desvpad
}

dpb = func_desvio_padrao(b)
dpb

dpb1 <- sd(b)
dpb1


# remover todos os objetos
rm(list=ls())

# criando uma matriz
m1 <- array(1:20, dim=c(4,5))
m1

m2 <- array(c(1:3, 1:3), dim=c(4,2))
m2

m3 <- cbind(m1,m2)
m3

m4 <- array(c(3:3, 3:3), dim=c(2,2))
m4

# erro devido ao numero de linhas diferentes
# m5 <- cbind(m3,m4)
# m5

# erro devido ao numero de colunas diferentes
# m6 <- rbind(m3,m4)
# m6

# usando o rbind
m7 <- rbind(m2,m4)
m7
