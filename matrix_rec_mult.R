# --- Fonction qui simule M ponts en iterant r fois notre recurrence ---
# ---- Stockage de chaque etat du pont en ligne avec cle binaire ----
source("rec_mult.R")
source("transfo_bin.R")

matrix_rec_mult <- function(M,N,r, p,x0) {
  ans <- matrix(0, M, r+1)
  for (i in seq_len(M)) {
    k <- transfo_bin(rec_mult(r, x0, N, p))
    ans[i,] <- k
  }
  return(ans)
}