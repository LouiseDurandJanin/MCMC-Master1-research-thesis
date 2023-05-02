# ----Fonction qui renvoie une realisation de MH avec bernoulli de parametre p -----
# --- La ligne i de la matrice de sortie correspond à l'etat du pont à l'iteration i ---
source("Q_p.R")
source("rec.R")

mh_sampler <- function(x0, r, p, N) {
  res <- x0
  mat <- matrix(0, r + 1, N)
  mat[1, ] <- diff(x0)
  for (i in seq_len(r)) {
    y_star <- rec(res, p, N)
    if (Q_p(res, y_star, p, N) > 0) {
      rho <- min(1, (Q_p(y_star, res, p, N) / Q_p(res, y_star, p, N)))
      b <- rbinom(1, 1, rho)
      res <- y_star * b + res * (1 - b)
    }
    mat[i + 1, ] <- diff(res)
  }
  return(mat)
}

