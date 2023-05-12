# --- Fonction qui renvoie une matrice des r premiere iterations de MH sur M ponts ---
# -- Ligne i de matrice output : r etats du ieme pont stockes sous forme de cles binaires --
source("transfo_bin.R")
source("mh_sampler.R")

matrix_MH <- function(M, N, r, x0, p) {
  ans <- matrix(0, M, r+1)
  for (i in seq_len(M)) {
    k <- transfo_bin(mh_sampler(x0, r, p, N))
    ans[i, ] <- k
  }
  return(ans)
}

