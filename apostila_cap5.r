#EXERCICIOS SEMANA 5 APOSTILA PROFESSORA JESSIKA
#1-
#receba um vetor v e escalar x e retorne vetor da forma ax

multvetor <- function(v, x) {
  n <- length(v)
  vx <- NULL
  for (i in 1:n) {
    vx[i] <- v[i] * x
  }
  return(vx)
}
multvetor(c(1, 2, 3, 4, 5), 2)
#RESOLVIDO

#2-
#receber dois vetores e retornar um vetor com a soma entre eles
somavetores <- function(v, c) {
  if (!is.numeric(v) || !is.numeric(c)) {
    stop("Ambos vetores devem conter apenas valores numericos inteiros.")
  }
  return(v + c)
}
a <- 1:9
b <- 1:9
somavetores(a, b)
#RESOLVIDO

#4-
#produto interno dois vetores
pinterno <- function(v, u) {
  resultado <- 0
  n <- length(v)
  for (i in 1:n) {
    resultado <- resultado + (v[i] * u[i])
  }
  return(resultado)
}
pinterno(a, b)
# RESOLVIDO

#5-
#
ortogonal <- function(u, v) {
  if (sum(u * v) == 0) {
    return("Os vetores são ortogonais.")
  } else {
    print("Os vetores não são ortogonais.")
  }
}
u <- c(1, 2, 3, 4)
v <- c(1, 2, 3, 4)
ortogonal(u, v)
#RESOLVIDO

#6-
#recebe uma matriz e um escalar e retorna matriz escalada
escalar <- function(u, k) {
  return(u * k)
}
u <- matrix(1:9, 3, 3)
escalar(u, 3)

#7-
#recebe duas matrizes e retorna a soma entre elas
somamat <- function(u, v) {
  return(u + v)
}
u <- matrix(1:9, 3, 3)
v <- matrix(1:9, 3, 3)
somamat(u, v)
#RESOLVIDO

#9-
#recebe uma matriz e retorna a transposta
transposta <- function(u) {
  r <- matrix(0, nrow(u), ncol(u))
  t <- nrow(u)
  y <- ncol(u)
  for (i in 1:t) {
    for (j in 1:y) {
      r[i, j] <- u[j, i]
    }
  }
  return(r)
}
u <- matrix(1:9, 3, 3)
transposta(u)
#RESOLVIDO

#10-
#recebe uma matriz e verifica se é simetrica
simetria <- function(u) {
  if (nrow(u) == ncol(u)) {
    return("A matriz é simétrica, pois a quantidade
    de linahs e colunas é igual.")
  } else {
    return("A matriz não é simétrica pois a quantidade
    de linhas e colunas é diferente.")
  }
}
u <- matrix(1:9, 3, 3)
u <- matrix(1:10, 5, 2)
simetria(u)
#RESOLVIDO

#11-
#recebe uma matriz e um vetor e retorna a multiplicação entre eles
multvetormat <- function(a1, vet) {
  if (nrow(a1) != length(vet)) {
    stop("O número de elementos do vetor deve ser igual a
    quantidade de linhas da matriz.")
  } else {
    vetor <- NULL
    n <- nrow(a2)
    a2 <- a1 * vet
    for (i in 1:n) {
      vetor <- c(vetor, sum(a2[i, ]))
    }
    return(vetor)
  }
}
a1 <- matrix(1:9, 3, 3)
vet <- c(3, 3, 3)
a1 * vet
multvetormat(a1, vet)
#RESOLVIDO

#12-
#recebe duas matrizes e retorna o produto entre elas
produto_matrizes <- function(f, g) {
  n1 <- nrow(f)
  n2 <- ncol(g)
  if (nrow(f) == ncol(g)) {
    k <- matrix(0, nrow(f), ncol(g))
    for (i in 1:n1) {
      for (j in 1:n2) {
        k[i, j] <- sum(f[i, ] * g[, j])
      }
    }
    return(k)
  } else {
    stop("A quantidade de linhas da primeira matriz precisa ser igual
    ao número de colunas da segunda matriz.")
  }
}
f <- matrix(c(1, 3, 11, 5, 7, 8, 6, 4, 2), 3, 3)
g <- matrix(c(6, 4, 1, 13, 4, 16, 10, 8, 9), 3, 3)
produto_matrizes(f, g)
#RESOLVIDO

#13-
a <- 4
b <- -3
v1 <- c(2, -3, -1, 5, 0, -2)
v2 <- c(3, 4, -1, 0, 1, 1)
v3 <- c(1, 2, 3, 4, 5)
v4 <- c(0, 1, 1)
m1 <- matrix(c(1, -1, 3, 0, 2, 1), 2, 3)
m2 <- matrix(c(0, -1, 1, -5, 1, 4, 3, -1, 0), 3, 3)
m3 <- matrix(c(3, -2, 3, 1, 10, -1), 3, 2)
m4 <- matrix(c(1, 0, 1, 1), 2, 2)
m5 <- matrix(c(3, 1, 0, 1, 1, 1, 3, 2, 0, 3, -5, 0, 1, 2, 0, 0), 4, 4)

a * v3
v1 + v2
#v3 - v1