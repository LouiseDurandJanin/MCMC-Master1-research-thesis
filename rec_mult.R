# ---Fonction qui stocker les r iterations de la recurrence d'initialisation x ---
#---- ponts stockes en increments ----

rec_mult <- function(r, x, N, p = 1 / 2) {
  res <- matrix(0, r + 1, N)
  res[1, ] <- diff(x)
  u <- sample(2:N, r, replace = TRUE)
  b <- rbinom(r, 1, p)
  for (i in seq_len(r)) {
    if (x[u[i] - 1] == x[u[i] + 1]) {
      x[u[i]] <-  x[u[i]] + 2 * (x[u[i] + 1] == (x[u[i]] + 1)) - 2 * b[i]
    }
    res[i + 1, ] <- diff(x)
    
  }
  return(res)
}