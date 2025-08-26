# Exercicio 7 do Projeto de PE

# Dúvidas - a diferença absoluta dá 0.439, o que é uma diferença bastante grande, o teta exato puseram como 3.4 mas o teta que me deu, o estimativa_tetha, deu-me 0.5593108, o que é bastante diferente, kinda extremely sussy
#           posso fazer aquilo de pôr uma bound inferior no valor do teta na função de mle
#           resultado final é 0.439 o que não tem 4 casas decimais, não te esqueças no moodle de pôr 0.4390 ! (Isto aplica-se para todos os exercícios do moodle, pôr as casas decimais certas para cada exercício!)


# Dados fornecidos
dados <- c(8.54, 4.76, 5.15, 4.96, 6.25, 7.22, 12.9, 6.04, 8.86, 4.88, 6.54, 4.53, 4.7, 5.38, 5.96, 5.17, 5.09, 5.11)
a <- 4.5


# Definição da função de densidade de probabilidade
pdf <- function(x, theta, a) {
  ifelse(x >= a, theta * x^(-theta - 1) * a^(theta), 0)
}

# Log de produto é igual à soma de logs
# Função de log-verossimilhança
log_verossimilhanca <- function(theta) {
  -sum(log(pdf(dados, theta, a)))
}


# Estimativa de Máxima Verossimilhança de teta
library(stats4)
resultado_mle <- mle(minuslogl = log_verossimilhanca,
                     start = list(theta = 3.4))

# Conseguir o teta
estimativa_theta <- coef(resultado_mle)
print(estimativa_theta)


# Função para integrar a função de densidade de probabilidade
integrate_pdf <- function(x, theta, a, p) {
  integral <- integrate(function(u) pdf(u, theta, a), lower = a, upper = x)$value
  return(integral - p)
}

# Valor do quantil de probabilidade p=0.25 com o teta estimado
quantil_estimado <- uniroot(integrate_pdf, interval = c(a, max(dados)), theta = estimativa_theta, a = a, p = 0.25)$root


# Valor do quantil de probabilidade p=0.25 com o teta exato
quantil_verdadeiro <- uniroot(integrate_pdf, interval = c(a, max(dados)), theta = 3.4, a = a, p = 0.25)$root


# E é isso que pedem no enunciado, do quantil

# Cálculo do desvio absoluto
desvio_absoluto <- abs(quantil_estimado - quantil_verdadeiro)

print(desvio_absoluto)


