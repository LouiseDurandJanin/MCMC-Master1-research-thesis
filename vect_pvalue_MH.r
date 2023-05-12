# --- Fonction qui crée un vecteur de pvaleur pour les sorties de MH---
# ----- Pvaleurs calcules toutes les 'lag' iterations jusqu'a r, avec M ponts -----
source("mu.R")
source("matrix_MH.R")

vect_pvalue <- function(M, N, r, p, x0, lag = 10) {
  rangs <- seq(10, r, by = lag)
  mat <- matrix_MH(M, N, r, x0, p)
  res <- rep(0, length(rangs))
  cpt <- 1
  mu <- mu(N)
  for (j in rangs) {
    freq <- table(mat[, j])
    p_MH <- c(freq, rep(0, length(mu) - length(freq)))
    res[cpt] <- chisq.test(p_MH, p = mu, rescale.p = TRUE)$p.value
    cpt = cpt + 1
  }
  return(res)
}