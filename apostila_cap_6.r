#1
#criar função recursiva que retorna n fatorial
n_fatorial <- function(n) {
    if (n < 0) {
        stop("n precisa ser um número natural.")
    }
    if (n == 0 || n == 1) {
        return(1)
    }
    return(n * n_fatorial(n - 1))
}