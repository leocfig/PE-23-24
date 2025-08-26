# Exercicio 5 do Projeto de PE

# Dúvidas - está certo?

# Definir parâmetros
n <- 23
r <- 300
m <- 170
limite <- 1.5
seed <- 1950

# Fixar a semente
set.seed(seed)

# Inicializar vetor para armazenar as proporções
proporcoes <- numeric(r)

#VER
for (i in 1:r) { # Matrix m * (n+1)
  Z <- matrix(rnorm(m * (n+1)), ncol = n+1)  # Gera variáveis normais
  T <- sqrt(n) * (Z[, 1] / sqrt(rowSums(Z[, -1]^2)))
  under_or_equal_limit <- ifelse(T <= limite, 1, 0)
  proporcao <- mean(under_or_equal_limit)
  proporcoes[i] <- proporcao
}

# Calcular a média das proporções como uma aproximação de p
p_aprox <- mean(proporcoes)

# Calcular o valor da função de distribuição t de R para T ≤ 1.5
valor_t <- pt(limite, df = n)

# Calcular a diferença absoluta entre a média das proporções e o valor obtido através da função de distribuição t de R
diferenca_absoluta <- abs(p_aprox - valor_t) * 100


# Imprimir o resultado final
cat("Resultado final (em %) arredondado a 5 casas decimais:", diferenca_absoluta, "\n")
