#QUESTÕES PROVA PROGRAMAÇÃO ESTATISTICA PRATICA
# 1 -
# a)
elementos_diferentes <- function(u, v) {
  if (length(u) != length(v)) {
    stop("Os vetores tem tamanhos diferentes.")
  }
  quantidade_elementos_distintos <- 0
  n <- length(u)
  for (i in 1:n) {
    if (u[i] != v[i]) {
      quantidade_elementos_distintos <- quantidade_elementos_distintos + 1
    }
  }
  return(quantidade_elementos_distintos)
}

# b)
elementos_diferentes(c(10, 3, 1, 2), c(1, 1, 10, 1))


#guardando todos os numeros primos menores que k em um vetor
primos_menores <- function(k) {
  p <- c(2)
  n <- 3
  while (n < k) {
    e_primo <- TRUE
    i <- 1
    while (i <= length(p)) {
      if (n %% p[i] == 0) {
        e_primo <- FALSE
        break
      }
      i <- i + 1
    }
    if (e_primo) {
      p <- c(p, n)
    }
    n <- n + 1
  }
  return(p)
}

print(primos_menores(20))