# --- Fonction qui effectue une iteration de couplage ---

coupl_step <- function(x, y, p = .5, N) {
  u <- sample(2:N, 1)
  b <- rbinom(1, 1, p)
  if (x[u - 1] == x[u + 1]) {
    x[u] <- x[u] + 2 * (x[u + 1] == (x[u] + 1)) - 2 * b
  }
  if (y[u - 1] == y[u + 1]) {
    y[u] <- y[u] + 2 * (y[u + 1] == (y[u] + 1)) - 2 * b
  }
  return(list(x = x, y = y))
}
