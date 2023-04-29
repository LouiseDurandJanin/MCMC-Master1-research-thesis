# --- Transforme les vecteurs d increments en vecteur de 0 et 1 puis donne sa cle binaire ---

transfo_bin <- function(x) {
  x[x == -1] <- 0
  return(x %*% 2^((ncol(x) - 1) : 0))
}
