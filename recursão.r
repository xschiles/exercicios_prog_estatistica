#AQUI COMEÇA PARTE DE RECURSÃO
#fatorial com algoritmo de recursão 1
fatorial <- function(n) {
  if (n < 0) {
    stop("n deve ser um inteiro não negativo.")
  }
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * fatorial(n - 1))
  }
}
fatorial(15)

#fatorial com recursão 2
fatorial <- function(n) {
  if (n == 0) {
    return(1)
  }
  return(n * fatorial(n - 1))
}
fatorial(6)

#MAX SEM USAR RECURSÃO
maximo <- function(vetor) {
  vmax <- vetor[1]
  n <- length(vetor)
  for (i in 2:n) {
    if (vetor[i] > vmax) {
      vmax <- vetor[i]
    }
  }
  return(vmax)
}
maximo(c(1:9))

#MAXIMO USANDO RECURSÃO
maior_rec <- function(v) {
  n <- length(v)
  if (n == 1) {
    return(v[1])
  }
  w <- v[2:n]
  max_w <- maior_rec(w)
  if (v[1] > max_w) {
    return(v[1])
  }
  return(max_w)
}
maior_rec(c(1:9))

#MINIMO USANDO RECURSÃO
menor_rec <- function(v) {
  n <- length(v)
  if (n == 1) {
    return(v[1])
  }
  w <- v[2:n]
  menor_w <- menor_rec(w)
  if (v[1] < menor_w) {
    return(v[1])
  }
  return(menor_w)
}
menor_rec(c(10:1))

#SOMA USANDO RECURSÃO
soma_rec <- function(v) {
  n <- length(v)
  if (n == 1) {
    return(v)
  } else {
    return(soma_rec(v[-n]) + v[n])
  }
}
v <- c(1:10)
soma_rec(v)

#MEDIA USANDO RECURSÃO
media_rec <- function(v) {
  n <- length(v)
  if (n == 1) {
    return(v)
  } else {
    return(((n - 1) * media_rec(v[-n]) + v[n]) / n)
  }
}
v <- c(1:1000)
media_rec(v)

#SEQUENCIA DE FIBONACCI
fibonacci <- function(n) {
  if (n == 1 || n == 2) {
    return(1)
  } else {
    return(fibonacci(n - 1) + fibonacci(n - 2))
  }
}
n <- 50
fibonacci(n)

tempo <- system.time({
  resultado <- fibonacci(50)
})

cat("Resultado:", resultado, "\nTempo de execução:",
    tempo["elapsed"], "segundos\n")

inicio <- Sys.time()
fibonacci <- function(n) {
  if (n == 1) {
    return(c(1))
  } else if (n == 2) {
    return(c(1, 1))
  } else {
    fib_seq <- c(1, 1)  # Os dois primeiros números da sequência
    for (i in 3:n) {
      fib_seq <- c(fib_seq, fib_seq[i - 1] + fib_seq[i - 2])
      cat("Fibonacci(", i, "):", fib_seq[i], "\n")  # Mostra o valor atual
    }
    return(fib_seq)
  }
}
# Medindo o tempo e mostrando a sequência
n <- 1000
resultado <- fibonacci(n)
fim <- Sys.time()
cat("\nTempo total:", fim - inicio, "segundos\n")

fatorial <- function(n) {
  if (n < 0) {
    stop("n precisa ser mais ou igual a zero.")
  }
  fat <- 1
  if (n == 0) {
    return(fat)
  }
  i <- 1
  while (i <= n) {
    fat <- fat * i
    i <- i + 1
  }
  return(fat)
}
fatorial(6)


maior <- function(v) {
  n <- length(v)
  max <- v[1]
  for (i in 2:n) {
    if (v[i] > max) {
      max <- v[i]
    }
  }
  return(max)
}
maior(c(9:1))

maior_recursivo <- function(v) {
  n <- length(v)
  if (n == 1) {
    return(v[1])
  }
  w <- v[2:n]
  max_w <- maior_recursivo(w)
  if (v[1] > max_w) {
    return(v[1])
  }
  return(max_w)
}
maior_recursivo(c(1, 9, 11, 24, 2, 5, 24.1, 7, 7, 2))

