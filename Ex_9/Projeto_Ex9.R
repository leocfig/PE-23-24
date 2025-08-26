# Exercicio 9 do Projeto de PE

# Dúvidas - está certo?
#           enquanto rejeitamos H0 quando a média amostral é maior que k, rejeitamos H1 quando a média amostral é menor ou igual a k. - é uma suposição baseada na lógica de um teste unicaudal à direita???


seed <- 4588
set.seed(seed) # Fixando a semente

lambda0 <- 2.40 # Parâmetro sob H0
lambda1 <- 2.65 # Parâmetro sob H1
k <- 2.623 # Valor de corte

n_simulations <- 5000
n_samples <- 100

# Função para calcular a média amostral
calc_mean <- function(sample) {
  mean(sample)
}

# Simulação
error_type_1 <- 0 # Erro do tipo 1: Rejeitar H0 quando H0 é verdadeira
error_type_2 <- 0 # Erro do tipo 2: Não rejeitar H0 quando H1 é verdadeira

for (i in 1:n_simulations) {
  # Gerar amostras sob H0
  sample_h0 <- rpois(n_samples, lambda0)
  
  # Gerar amostras sob H1
  sample_h1 <- rpois(n_samples, lambda1)
  
  # Calcular médias amostrais
  mean_h0 <- calc_mean(sample_h0)
  mean_h1 <- calc_mean(sample_h1)
  
  # Teste de hipóteses
  if (mean_h0 > k) {
    error_type_1 <- error_type_1 + 1
  }
  
  if (mean_h1 <= k) {
    error_type_2 <- error_type_2 + 1
  }
}

# Frequências relativas
freq_error_type_1 <- error_type_1 / n_simulations
freq_error_type_2 <- error_type_2 / n_simulations

# Calculando o quociente entre a probabilidade de erro de segunda espécie e a probabilidade de erro de primeira espécie
quociente <- freq_error_type_2 / freq_error_type_1

# Imprimir resultados
cat("Frequência relativa do erro do tipo 1:", freq_error_type_1, "\n")
cat("Frequência relativa do erro do tipo 2:", freq_error_type_2, "\n")
cat("Quociente entre a probabilidade de erro de segunda espécie e a probabilidade de erro de primeira espécie:", quociente, "\n")


