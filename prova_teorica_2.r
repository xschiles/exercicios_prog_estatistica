#1
calculo <- function(n) {
  if (n == 1) {
    return(1)
  }
  return((n ^ 2) * calculo(n - 1))
}
calculo(3)


#2
sub_vetor_menor_recursivo <- function(v, x) {
  if (length(v) == 0) {
    return(NULL)
  }
  if (length(v) == 1) {
    if (v[1] < x) {
      return(v)
    } else {
      return(NULL)
    }
  }
  restante <- sub_vetor_menor_recursivo(v[-1], x)
  if (v[1] < x) {
    w <- c(v[1], restante)
  } else {
    w <- restante
  }
  return(w)
}
sub_vetor_menor_recursivo(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5)
