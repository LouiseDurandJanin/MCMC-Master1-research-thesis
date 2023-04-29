# ----Fonction qui renvoie une realisation de MH proba p prise les r premiere iterations de MH -----
source("Q_p.R")
source("rec.R")

#----Une realisation de MH proba p prise au rang r------
mh_sampler <- function(x0, r, p, N) {
  res <- x0
  mat <- matrix(0, r + 1, N)
  mat[1,] <- diff(x0)
  for (i in seq_len(r)) {
    y_star <- rec(res, p, N)
    if (Q_p(res, y_star, p, N) > 0) {
      rho <- min(1, (Q_p(y_star, res, p, N) / Q_p(res, y_star, p, N)))
      b <- rbinom(1, 1, rho)
      res <- y_star * b + res * (1 - b)
    }
    mat[i + 1,] <- diff(res)
  }
  return(mat)
}
mh_sampler(enveloppe(6,TRUE), 500, 3/4, 6)

