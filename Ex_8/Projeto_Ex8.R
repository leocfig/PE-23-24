# Exercicio 8 do Projeto de PE

# Dúvidas - está certo?
#           eles no enunciado pedem o valor da diferença, mas não em valor absoluto? Se calhar é por ser óbvio que o intervalo calculado em 2 é o maior dos dois, porque o passo 3 é literalmente arranjar outro intervalo que tenha uma amplitude menor do que a do intervalo de 2.



# Instalar e carregar o pacote pracma
install.packages("pracma")
library(pracma)

# Definir os dados da amostra
amostra <- c(34.0, 39.5, 33.2, 38.1, 29.9, 37.4, 32.1, 36.5, 31.4, 34.1, 33.1, 31.5, 33.9, 33.9)
n <- 8
seed <- 1820

set.seed(seed)


# Amostra aleatória de tamanho n=8, sem reposição.
amostra_aleatoria <- sample(amostra, n, replace = FALSE)

# Calcular a variância amostral
variancia_amostral <- var(amostra_aleatoria)


# Calcular os quantis de probabilidade - "Quando estamos construindo um intervalo de confiança para a variância de uma distribuição normal, utilizamos a distribuição X elevado a 2 (qui-quadrado)"
gamma <- 0.95
a <- qchisq((1 - gamma) / 2, df = n - 1)
b <- qchisq((1 + gamma) / 2, df = n - 1)

# Calcular o intervalo de confiança para a variância
intervalo_confianca <- (n - 1) * variancia_amostral / c(b, a)

# Calcular a amplitude do intervalo de confiança
amplitude_intervalo_confianca <- intervalo_confianca[2] - intervalo_confianca[1]



# Defining the functions for the equations
f <- function(x) {
  c(
    pchisq(x[2], df = n - 1) - pchisq(x[1], df = n - 1) - gamma,
    dchisq(x[2], df = n + 3) - dchisq(x[1], df = n + 3)
  )
}

x0 <- c(a, b)
# Encontrando os quantis de probabilidade c e d usando fsolve
sol <- fsolve(f, x0)
c <- sol$x[1]
d <- sol$x[2]


# Calcular o novo intervalo de confiança para σ²
novo_intervalo_confianca <- ((n - 1) * variancia_amostral) / c(d, c)

# Calcular a amplitude do novo intervalo de confiança
amplitude_novo_intervalo <- novo_intervalo_confianca[2] - novo_intervalo_confianca[1]


# Diferença entre as amplitudes dos intervalos de confiança
dif <- amplitude_intervalo_confianca - amplitude_novo_intervalo   # É o maior - menor, e sabemos qual o maior porque o objetivo do passo 3 era literalmente diminuir o tamanho da amplitude do intervalo de confiança obtido em 2


print(dif)


