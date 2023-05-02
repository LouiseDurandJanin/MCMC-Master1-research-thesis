# --- Stocke dans une matrice m ponts obtenus a coalescence ---


matrix_couplage <- function(M, N, p) {
  ans <- matrix(0, M, N)
  x0 <- enveloppe(N, TRUE)
  y0 <- enveloppe(N, FALSE)
  for (i in seq_len(M)) {
    k <- couplage(x0, y0, p, N)
    ans[i, ] <- diff(k$x)
  }
  return(ans)
}