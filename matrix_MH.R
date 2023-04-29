# --- Fonction qui renvoie une matrice des r premiere iterations de MH sur M ponts ---
source("transfo_bin.R")
source("mh_sampler.R")

matrix_MH <- function(M, N, r, x0, p) {
  ans <- matrix(0, M, N)
  for (i in seq_len(M)) {
    k <- transfo_bin(mh_sampler(r, x0, N, p))
    ans[i, ] <- k
  }
  return(ans)
}