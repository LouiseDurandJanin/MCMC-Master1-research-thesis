# --- Methode optimale pour simuler M ponts avec le rejet ---
# --- on simule que M*(proba_accept) suivant l'uniforme sur E pour en moyenne passer 1 fois dans while ---

source("proba_accept_rejet.R")

matrix_rejet <- function(M, N) {
  ans <- c()
  p_accept <- ceiling(proba_accept_rejet(N))
  left <- M
  i <- 0
  while (left > 0) {
    x <- matrix(sample(c(-1, 1), N * left * p_accept, replace = TRUE), nrow = left * p_accept)
    ans <- rbind(ans, x[rowSums(x) == 0,])
    left <- M - nrow(ans)
    i <- i + 1
  }
  return(ans)
}
