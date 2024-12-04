#LISTA DE MONITORIA
#1.
resultado <- function(salario, gasto_mensal) {
  gma <- "gastando mais que recebe"
  gme <- "gastando menos que recebe"
  gtu <- "gastando tudo que recebe"
  result <- salario - gasto_mensal
  if (result < 0) return(gma)
  if (result > 0) return(gme)
  if (result == 0) return(gtu)
  return(result)
}
resultado(1000, 2000)

#2.
mm <- function(b, n, m) {
  menor <- b
  maior <- n
  if (b < n && b < m) {
    menor <- b
    if (n < m) {
      maior <- m
    } else {
      maior <- n
    }
  }
  if (n < b && n < m) {
    menor <- n
    if (b < m) {
      maior <- m
    } else {
      maior <- b
    }
  }
  if (m < n && m < b) {
    menor <- m
    if (n < b) {
      maior <- b
    } else {
      maior <- n
    }
  }
  return(paste(menor, maior))
}
mm(31, 31.01, 31.02)

#3.
z <- function(vet) {
  maior <- vet[1]
  menor <- vet[1]
  for (i in 2:length(vet)) {
    if (vet[i] > maior) {
      maior <- vet[i]
    } else if (vet[i] < menor) {
      menor <- vet[i]
    }
  }
  return(paste(menor, maior))
}
z(c(543, 54, 67543, 675, 32, 2, 54, 65, 56, 34, 32))

#4.
salarios <- function(salario) {
  salario_final <- 0
  if (salario <= 300) {
    salario_final <- salario + salario * 0.30
  }
  if (salario > 300 && salario <= 800) {
    salario_final <- salario + salario * 0.20
  }
  if (salario > 800 && salario <= 1500) {
    salario_final <- salario + salario * 0.10
  }
  if (salario > 1500) {
    salario_final <- salario + salario * 0.05
  }
  return(paste(salario, salario_final - salario, salario_final))
}
salarios(2000)