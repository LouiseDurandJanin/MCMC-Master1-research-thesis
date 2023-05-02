# --- Processus de couplage avec une Bernoulli de parametre p, qui s arrete a coalescence --- 


couplage <- function(x , y , p , N) {
  i <- 0
  u <- c()
  while (!identical(x, y)) {
    temp <- coupl_step(x, y, p, N)
    x <- temp$x
    y <- temp$y
    i <- i + 1
  }
  return(list(x = x, cpt = i))
}