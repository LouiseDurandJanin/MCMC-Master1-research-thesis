# --- Une iteration de la recurrence ---


rec <- function(x, p, N) {
  u <- sample(2:N, 1)
  b <- rbinom(1, 1, p)
  if (x[u - 1] == x[u + 1]) {
    x[u] <- x[u] + 2 * (x[u + 1] == (x[u] + 1)) - 2 * b
  }
  return(x)
}


