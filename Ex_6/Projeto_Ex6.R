# Exercicio 6 do Projeto de PE

# Dúvidas - está certo?
#           aquela dúvida se usamos 1 - pgamma ou pgamma com lower.tail = FALSE


n <- 30
a <- 4
seed <- 1948
rept <- 1000
l <- 90

set.seed(seed)


# Abordagem 1: Valor Simulado - está mal?

# Gerando uma matriz de 1000 linhas (as amostras) e 30 colunas (as componentes eletrónicas) - como o vetor de entrada possui 30000 elementos e queremos que a matriz tenha 30 colunas, a função matrix calcula automaticamente que o número de linhas deve ser 30000/30 = 1000, e assim cria uma matriz com 1000 linhas e 30 colunas. 
#amostras <- matrix(rexp(rept * n, rate = 1/a), ncol = n)

# Calculando o valor simulado de Y para cada amostra - O vetor valores_simulados_Y é unidimensional e possui 1000 posições.
#valores_simulados_Y <- rowSums(amostras)

# Calculando a proporção de valores simulados de Y que são maiores que 90 - Como os valores booleanos são tratados como 1 para TRUE e 0 para FALSE, a média dos valores booleanos é a proporção de TRUE no vetor. 
#proporcao_simulada <- mean(valores_simulados_Y > 90)



# Abordagem 1: Valor Simulado
aceites <- 0
for (i in 1:rept) {
  aceites <- aceites + (sum(rexp(n, rate = 1/a)) > l)
}

proporcao_simulada <- aceites / rept

# Abordagem 2: Valor Exato

# Calculando a probabilidade exata usando a distribuição gama - Como queremos calcular a probabilidade de que a duração total das 30 componentes eletrônicas seja maior que 90, precisamos subtrair a probabilidade acumulada de que seja menor ou igual a 90 da probabilidade total, que é 1.
#probabilidade_exata <- 1 - pgamma(q = l, shape = n, rate = 1/a)
probabilidade_exata <- pgamma(q = l, shape = n, rate = 1/a, lower.tail = FALSE)


# Diferença Percentual

# Calculando a diferença percentual entre os resultados das duas abordagens
diferenca_percentual <- abs(proporcao_simulada - probabilidade_exata) * 100



# Imprimir o resultado final
cat("Resultado final (em %) arredondado a 4 casas decimais:", diferenca_percentual, "\n")


