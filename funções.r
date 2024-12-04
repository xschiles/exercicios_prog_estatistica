#NESTE CÓDIGO ESTÁ TODO O MEU APRENDIZADO PRÁTICO COM DEFINICAÇÃO
#DE FUNÇÕES NO R. RESOLUÇÃO DE EXERCÍCIOS DA PROFESSORA JÉSSIA KUBRUSKY

funcao <- function(n) {
  pares <- NULL
  for (i in 1:n) {
    pares <- c(pares, 2 * i)
  }
  return(pares)
}
funcao(10)
funcao(1000)
funcao(50)

#questao1
menor <- function(n, x) {
  if (n < x) {
    return(n)
  } else {
    return(x)
  }
}
menor(5, 2)
menor(2, 5)

menor(2.6, 2.59)

menor <- function(n, x, v) {
  if (n < x && n < v) {
    return(n)
  }
  if (x < n && x < v) {
    return(x)
  } else {
    return(v)
  }
}
menor(3, 4, 2)

menor <- function(vetor) {
  n <- length(vetor)
  minimo <- vetor[1]
  for (i in 1:n){
    if (vetor[i] < minimo) {
      minimo <- vetor[i]
    }
  }
  return(minimo)
}
maximo <- function(dados) {
  n <- length(dados)
  max <- dados[1]
  for (i in 1:n) {
    if (dados[i] > max) {
      max <- dados[i]
    }
  }
  return(max)
}

menor(c(234, 123, 355, 345, 23, 43, 234, 45, 345, 34523, 32, 234))
asd <- 1:10
menor(asd)


posit <- function(vetor) {
  p <- 0
  positivos <- NULL
  n <- length(vetor)
  for (i in 1:n) {
    if (vetor[i] > 0) {
      positivos <- paste(vetor[i], positivos)
      p <- p + 1
    }
  }
  return(positivos)
  return(p)
}
v <- c(1.0, 3.2, -2.1, 10.6, 0.0, -1.7, -0.5)
posit(v)

p <- 0
posit <- function(vetor) {
  n <- length(vetor)
  for (i in 1:n) {
    if (vetor[i] > 0) {
      p <- p + 1
    }
  }
  return(p)
}
posit(c(2, 3, 4, 5, 6, 7, 78, -1, -2, -1, -5, 9))

#5.
#a.
menores <- function(n, m) {
  vet2 <- NULL
  vet <- 1:m
  for (i in 1:m) {
    if (m %% vet[i] == 0) {
      vet2[i] <- vet[i]
    }
  }
  return(head(vet2[!is.na(vet2)], n))
}
menores(3, 10)

#c.
#errado
mult <- function(m, k) {
  h <- NULL
  n <-  1:k
  for (i in 1:k) {
    if (m %% n[i] == 0) {
      h[i] <- n[i]
      print("é multiplo")
    }
  }
  return(h[!is.na(h)])
}
mult(10, 20)

#certo
mult2 <- function(m, k) {
  h <- NULL
  for (i in 1:(k - 1)) {
    if (i * m < k) {
      h <- c(h, i * m)
    } else {
      break
    }
  }
  return(h)
}
mult2(10, 200)

#6.
#a.
matriz <- function(n) {
  if (n < 0) {
    break
  } else {
    l <- matrix(0, n, n, byrow = TRUE)
    for (i in seq_len(ncol(l))) {
      l[i, ] <- i
    }
    return(l)
  }
}
matriz(3)

#b.
matriz <- function(n) {
  if (n < 0) {
    break
  } else {
    l <- matrix(0, n, n, byrow = TRUE)
    for (i in seq_len(ncol(l))) {
      l[, i] <- i
    }
    return(l)
  }
}
matriz(3)

#c.
matriz <- function(n) {
  if (n < 0) {
    break
  } else {
    l <- matrix(0, n, n, byrow = TRUE)
    for (i in seq_len(ncol(l))) {
      l[i, i] <- i
    }
    return(l)
  }
}
matriz(10)

#7.
#a.
matriz <- function(n) {
  l <- matrix(0, length(n), length(n), byrow = TRUE)
  for (i in seq_len(ncol(l))) {
    l[i, i] <- n[i]
  }
  return(l)
}
matriz(c(4:8))


#b.
matriz <- function(n) {
  l <- matrix(0, length(n), length(n), byrow = TRUE)
  for (i in seq_len(ncol(l))) {
    l[, i] <- n[i]
  }
  return(l)
}
matriz(c(3:1))

#c.
matriz <- function(n) {
  l <- matrix(0, length(n), length(n), byrow = TRUE)
  for (i in seq_len(ncol(l))) {
    l[i, ] <- n[i]
  }
  return(l)
}
matriz(c(3:1))

version

#8
#a.
# an = a₁ + (n - 1) * r
x <- function(n) {
  g <- NULL
  c <- 1
  for (i in 1:10) {
    g[i] <- c(n + (c - 1) * 3)
    c <- c + 1
  }
  return(g)
}
x(5)

#b.
x <- function(n, r) {
  g <- NULL
  c <- 1
  for (i in 1:10) {
    g[i] <- c(n + (c - 1) * r)
    c <- c + 1
  }
  return(g)
}
x(5, 3)

#c.
x <- function(n, r, v) {
  g <- NULL
  c <- 1
  for (i in 1:v) {
    g[i] <- c(n + (c - 1) * r)
    c <- c + 1
  }
  return(g)
}
x(5, 3, 20)

#d.
x <- function(n, r, v) {
  g <- NULL
  soma <- 0
  c <- 1
  for (i in 1:v) {
    g[i] <- c(n + (c - 1) * r)
    c <- c + 1
    soma <- soma + g[i]
  }
  return(soma)
}
x(5, 3, 20)

#9.
#a.
#F(n)=F(n−1)+F(n−2)
fib <- function(n) {
  if (n == 1) {
    return(c(0))
  }
  if (n == 2) {
    return(c(0, 1))
  }
  vetor <- c(0, 1)
  for (i in 3:n) {
    vetor[i] <- vetor[i - 1] + vetor[i - 2]
  }
  return(vetor)
}
fib(10)

#b.
#ERRADO
fib1 <- function(k) {
  if (k == 1) {
    return(c(0))
  }
  if (k == 2) {
    return(c(0, 1))
  }
  vetor <- c(0, 1)
  for (i in 3:(k - 1)) {
    vetor[i] <- vetor[i - 1] + vetor[i - 2]
    if (vetor[i] > k) {
      break
    }
  }
  return(vetor)
}
fib1(35)

#CERTO
fib1 <- function(k) {
  if (k == 0) {
    return(integer())
  }
  if (k == 1) {
    return(c(0))
  }
  if (k == 2) {
    return(c(0, 1))
  }
  vetor <- c(0, 1)
  while (TRUE) {
    proximo <- vetor[length(vetor)] + vetor[length(vetor) - 1]
    if (proximo >= k) {
      break
    }
    vetor <- c(vetor, proximo)
  }
  return(vetor)
}
fib1(35)

#c.
fib1 <- function(k) {
  if (k == 0) {
    return(integer())
  }
  if (k == 1) {
    return(c(0))
  }
  if (k == 2) {
    return(c(0, 1))
  }
  vetor <- c(0, 1)
  c <- 2
  while (TRUE) {
    proximo <- vetor[length(vetor)] + vetor[length(vetor) - 1]
    if (proximo >= k) {
      break
    }
    vetor <- c(vetor, proximo)
    c <- c + 1
  }
  print(vetor)
  return(c)
}
fib1(0)
fib1(1)
fib1(2)
fib1(3)

#10.
#b.
f <- function(n) {
  v <- NULL
  for (i in 1:n) {
    v[i] <- i
  }
  return(v)
}
vet <- f(1)
vet
f(11)

#11.
#xk bh x0 * q ^ k
#a.
pg1 <- function(x0, q, n) {
  termos <- x0 * q ^ (0:(n - 1))
  return(termos)
}
pg1(2, 3, 10)

#b.
pg2 <- function(x0, q, n) {
  termos <- x0 * q ^ (0:(n - 1))
  print(sum(termos))
  return(termos)
}
pg2(2, 3, 5)

#c.
pg2 <- function(x0, q, n) {
  termos <- x0 * q ^ (0:(n - 1))
  print(sum(termos))
  return(termos)
}
pg2(0.5, 0.5, 10)

#d.
pg2 <- function(x0, q, n) {
  termos <- x0 * q ^ (0:(n - 1))
  print(sum(termos))
  return(termos)
}
pg2(0.5, 0.5, 30)

#12.
#b.
salario <- function(s, n) {
  vetor <- c(s)
  for (i in 1:n) {
    vetor <- c(vetor, vetor[i - 1] * 2)
  }
  return(vetor)
}
salario(0.01, 35)

#c.
salario <- function(s, n) {
  vetor <- c(s)
  for (i in 1:n) {
    vetor <- c(vetor, vetor[i - 1] * 2)
  }
  return(vetor)
}
salario(0.01, 22)

#13. Questão Desafio.
#a.
fat <- function(num) {
  factors <- c()
  div <- 2
  while (num > 1) {
    while (num %% div == 0) {
      factors <- c(factors, div)
      num <- num / div
    }
    div <- div + 1
  }
  return(factors)
}
fat(140)

#b.
is_prime <- function(k) {
  if (k <= 1) {
    return(FALSE)
  }
  if (k == 2) {
    return(TRUE)
  }
  for (i in 2:sqrt(k)) {
    if (k %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
is_prime(1)
is_prime(2)
is_prime(3)
is_prime(11)
is_prime(30)


#c.
primo <- function(n) {
  if (n == 0) {
    return(FALSE)
  }
  vetor <- c()
  c <- 2
  while (length(vetor) < n) {
    if (is_prime(c)) {
      vetor <- c(vetor, c)
    }
    c <- c + 1
  }
  return(vetor)
}
primo(50)


#achar o maximo
f <- function(n) {
  g <- length(n)
  max <- n[1]
  for (i in 1:g) {
    if (n[i] > max) {
      max <- n[i]
    }
  }
  return(max)
}
d <- round(runif(5, 10, 20), 0)
d
f(d)

#achar o minimo
s <- function(n) {
  g <- length(n)
  min <- n[1]
  for (i in 1:g) {
    if (n[i] < min) {
      min <- n[i]
    }
  }
  return(min)
}
d <- round(runif(5, 20, 30), 0)
d
s(d)

media <- function(v) {
  md <- sum(v) / length(v)
  print(sum(v))
  return(md)
}

mean <- function(v) {
  n <- length(v)
  soma <- 0
  i <- 1
  while (i <= n) {
    soma <- soma + v[i]
    i <- i + 1
  }
  media <- soma / n
  return(media)
}
mean(1:10)

media(1:10)
sort(c(5, 1, 4, 7, 3, 1))

mediana <- function(v) {
  n <- length(v)
  v_o <- sort(v)
  if (n %% 2 == 0) {
    a <- (v_o[n / 2] + v_o[(n / 2) + 1]) / 2
  } else {
    a <- v_o[(n + 1) / 2]
  }
  return(a)
}

quartis <- function(v) {
  n <- length(v)
  v_o <- sort(v)
  if (n %% 2 == 0) {
    k <- n / 2
    j <- k + 1
  } else {
    k <- (n - 1) / 2
    j <- k + 2
  }
  v_1 <- v_o[1:k]
  v_2 <- v_o[j:n]
  q_1 <- mediana(v_1)
  q_2 <- mediana(v)
  q_3 <- mediana(v_2)
  return(paste(q_1, q_2, q_3))
}
quartis(1:100)

a <- c(1:10)
a
k <- length(a) / 2
k
p <- a[1:k]
p

quartis <- function(v) {
  n <- length(v)
  v_o <- sort(v)

  if (n %% 2 == 0) {
    # For even n, divide the dataset into two equal halves
    k <- n / 2
    v_1 <- v_o[1:k]
    v_2 <- v_o[(k + 1):n]
  } else {
    # For odd n, the median is already the second quartile
    k <- (n - 1) / 2
    v_1 <- v_o[1:k]
    v_2 <- v_o[(k + 2):n]
  }

  q_1 <- mediana(v_1)
  q_2 <- mediana(v)
  q_3 <- mediana(v_2)

  return(paste(q_1, q_2, q_3))
}
quartis(1:100)

f <- function(n) {
  for (i in 1:n) {
    pares <- c(NULL, 2 * i)
  }
  return(pares)
}
f(1:10)

f <- function(n) {
  if (length(n) == 0 && class(n) != "numeric") {
    stop("the vector with data must have length bigger than zero
    and it's class must be numeric")
  }
  vetor <- NULL
  for (i in seq_along(n)) {
    if (n[i] %% 2 == 0) {
      vetor <- c(vetor, n[i])
    }
  }
  return(vetor)
}
f(1)


#input word1 = abc
#input word2 = def
#output adbecf
misturar <- function(word1, word2) {
  result <- ""
  len1 <- nchar(word1)
  len2 <- nchar(word2)
  min_len <- min(len1, len2)

  for (i in 1:min_len) {
    result <- paste0(result, substr(word1, i, i), substr(word2, i, i))
  }

  if (len1 > min_len) {
    result <- paste0(result, substr(word1, min_len + 1, len1))
  } else if (len2 > min_len) {
    result <- paste0(result, substr(word2, min_len + 1, len2))
  }
  return(result)
}

misturar("abc", "def")

#-------------------------------------------------------------------------
#MULTIPLICAÇÃO DE VETOR POR ESCALAR
vetor <- function(v, a) {
  n <- length(v)
  w <- NULL
  i <- 1
  while (i <= n) {
    w <- c(w, a * v[i])
    i <- i + 1
  }
  return(w)
}
g <- 1:10
vetor(g, 4)

#SOMA ENTRE VETORES
soma <- function(v, u) {
  n <- length(v)
  k <- length(u)
  if (n != k) {
    stop("Os vetores precisam ter as mesmas dimensões")
  }
  w <- NULL
  i <- 1
  while (i <= n) {
    w[i] <- v[i] + u[i]
    i <- i + 1
  }
  return(w)
}
vetor1 <- 1:10
vetor2 <- 1:10
soma(vetor1, vetor2)

#SUBTRAÇÃO ENTRE VETORES
subtracao <- function(v, u) {
  n <- length(v)
  k <- length(u)
  if (n != k) {
    stop("Os vetores precisam ter as mesmas dimensões")
  }
  w <- NULL
  i <- 1
  while (i <= n) {
    w[i] <- v[i] - u[i]
    i <- i + 1
  }
  return(w)
}
vetor1 <- 1:10
vetor2 <- 1:10
subtracao(vetor1, vetor2)

#PRODUTO INTERNO
produto <- function(v, u) {
  n <- length(v)
  k <- length(u)
  if (n != k) {
    stop("Os vetores precisam ter as mesmas dimensões")
  }
  p <- 0
  i <- 1
  while (i <= n) {
    p <- p + v[i] * u[i]
    i <- i + 1
  }
  return(p)
}
vetor1 <- 1:10
vetor2 <- 1:10
produto(vetor1, vetor2)

#MULTIPLICAÇÃO DE MATRIZ POR ESCALAR
escalar <- function(a1, a) {
  n <- nrow(a1)
  m <- ncol(a1)
  m1 <- matrix(, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      m1[i, j] <- a * a1[i, j]
    }
  }
  return(m1)
}
a1 <- matrix(1:9, 3, 3)
a <- 3
escalar(a1, a)

#SOMA ENTRE MATRIZES
somatrizes <- function(a2, b2) {
  n <- nrow(a2)
  m <- ncol(a2)
  l <- nrow(b2)
  c <- ncol(b2)
  if (n != l || m != c)
    stop("As matrizes tem dimensões diferentes.")
  m2 <- matrix(, n, m)
  i <- 1
  j <- 1
  for (i in 1:n) {
    for (j in 1:l) {
      m1[i, j] <- a2[i, j] + b2[i, j]
    }
  }
  return(m2)
}
a2 <- matrix(1:9, 3, 3)
b2 <- matrix(1:9, 3, 3)
somatrizes(a2, b2)

#SUBTRAÇÃO ENTRE MATRIZES
subtrizes <- function(a3, b3) {
  n <- nrow(a3)
  m <- ncol(a3)
  l <- nrow(b3)
  c <- ncol(b3)
  if (n != l || m != c)
    stop("As matrizes tem dimensões diferentes.")
  m3 <- matrix(, n, m)
  i <- 1
  j <- 1
  for (i in 1:n) {
    for (j in 1:l) {
      m3[i, j] <- a3[i, j] - b3[i, j]
    }
  }
  return(m3)
}
a3 <- matrix(1:9, 3, 3)
b3 <- matrix(1:9, 3, 3)
subtrizes(a3, b3)

#TRANSPOSIÇÃO DE MATRIZES
transpor <- function(a4) {
  n <- nrow(a4)
  m <- ncol(a4)
  m4 <- matrix(, n, m)
  i <- 1
  j <- 1
  for (i in 1:n) {
    for (j in 1:m) {
      m4[i, j] <- a4[j, i]
    }
  }
  return(m4)
}
a4 <- matrix(1:9, 3, 3)
transpor(a4)

#MULTIPLICAÇÃO ENTRE MATRIZ E VETOR
multmv <- function(a5, v5) {
  n <- nrow(a5)
  m <- ncol(a5)
  k <- length(v5)
  if (m != k) {
    stop("O número de colunas e o tamanho do vetor precisam ser iguais")
  }
  w <- numeric(n)
  for (i in 1:n) {
    w[i] <- sum(a5[i, ] * v5)
  }
  return(w)
}

a5 <- matrix(1:9, 3, 3)
v5 <- c(2, 2, 2)
multmv(a5, v5)

#MULTIPLICAÇÃO ENTRE MATRIZES
multmat <- function(a6, b6) {
  n <- nrow(a6)
  m <- ncol(a6)
  l <- nrow(b6)
  c <- ncol(b6)
  if (m != l) {
    stop("As matrizes tem dimensões diferentes")
  }
  print(a6)
  print(b6)
  m6 <- matrix(, n, c)
  i <- 1
  j <- 1
  for (i in 1:n) {
    for (j in 1:c) {
      m6[i, j] <- sum(a6[i, ] *  b6[, j])
      print(m6)
    }
  }
  return(m6)
}
a6 <- matrix(1:9, 3, 3)
b6 <- matrix(1:9, 3, 3)
multmat(a6, b6)