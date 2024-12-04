#EXERCICIOS LISTA 5 MONITORIA
#1-
matriz <- matrix(1:90, 10, 9)
matriz
sum(matriz)
mean(sum(matriz) / length(matriz))
mediamatriz <- function(matriz) {
  return(mean(sum(matriz) / length(matriz)))
}
mediamatriz(matriz)

#2-
#recebe uma matriz, indice da col e retorna variancia da col
coluna_variancia <- function(matriz, e) {
  return(var(matriz[, e]))
}
coluna_variancia(matrix(1:81, 9, 9), 6)

#3-
#recebe uma matriz e retorna a matriz inversa dessa matriz
inversa <- function(a) {
  if (nrow(a) == ncol(a)) {
    a_det <- det(a)
    if (a_det != 0) {
      a_inv <- solve(a)
      print(a_inv)
    } else {
      print("a matriz nao tem inversa,
      pois o determinante é igual a zero.")
    }
  } else {
    print("não é possivel calcular a inversa pois a matriz não é quadrada.")
  }
}
a <- matrix(c(23, 4, 5, 34, 2, 1, 4, 35, 34), 3, 3)
a
inversa(a)
#RESOLVIDO

#4-
#recebe a matriz e retorna a matriz diagonal
diagonal <- function(m) {
  if (nrow(m) == ncol(m)) {
    t <- nrow(m)
    y <- ncol(m)
    m1 <- matrix(0, nrow(m), ncol(m))
    for (i in 1:t) {
      for (j in 1:y) {
        if (i == j) {
          m1[i, j] <- m[i, j]
        }
      }
    }
    return(m1)
  } else {
    print("A matriz precisa ser quadrada.")
  }
}
m <- matrix(1:9, 3, 3)
m
diagonal(m)

#SE VOCÊ QUISER A DIAGONAL SECUNDARIA:
diagonal_secundaria <- function(m) {
  if (nrow(m) == ncol(m)) {
    m1 <- matrix(0, nrow(m), ncol(m))
    n <- nrow(m)
    for (i in 1:n) {
      j <- n - i + 1
      m1[i, j] <- m[i, j]
    }
    return(m1)
  } else {
    stop("A matriz deve ser quadrada.")
  }
}
m <- matrix(1:9, 3, 3, byrow = TRUE)
m
diagonal_secundaria(m)

#5-
#recebe duas matrizes e retorna a multiplicação entre elas
a <- matrix(1:9, 3, 3)
a
b <- matrix(11:19, 3, 3)
b

multmatrizes <- function(a, b) {
  return(a %*% b)
}
multmatrizes(a, b)

#6-
#recebe uma matriz e retorna o resultado da exponenciação
#de todos os elementos da matriz
expo <- function(m1, ex) {
  resultado <- matrix(0, nrow(m1), ncol(m1))
  t <- nrow(m1)
  y <- ncol(m1)
  for (i in 1:t) {
    for (j in 1:y) {
      resultado[i, j] <- m1[i, j] ^ 2
    }
  }
  return(resultado)
}
m1 <- matrix(1:9, 3, 3)
m1
expo(m1, 2)

#7-
#recebe uma matriz e retorna o maior valor em cada linha
maiorlinha <- function(m2) {
  resultado <- numeric(nrow(m2))
  t <- nrow(m2)
  y <- ncol(m2)
  for (i in 1:t) {
    maior <- m2[i, 1]
    for (j in 1:y) {
      if (m2[i, j] > maior) {
        maior <- m2[i, j]
      }
    }
    print(paste("O maior elemento da linha", i, "é", maior))
    resultado[i] <- maior
  }
  return(resultado)
}
m2 <- matrix(1:9, 3, 3)
m2

maiorlinha(m2)

#8-
#recebe uma matriz e retorna a soma dos elementos abaixo da diagonal principal
soma_abaixo_diagonal <- function(m3) {
  soma <- 0
  for (i in 2:nrow(m3)) {
    for (j in 1:(i - 1)) {
      soma <- soma + m2[i, j]
    }
  }
  return(soma)
}
m3 <- matrix(1:9, 3, 3)
m3
somaAbaixoDiagonal(m3)