#--Méthode naïve qui génère m ponts de taille N par la méthod du rejet--

mat_rejet_naif <- function(m, N) {
  x <- matrix(0, m, N)
  # p <- rep(0, m)
  for (i in seq_len(m)) {
    temp <- rejet(N)
    x[i, ] <- temp$x
    # p[i] <- temp$prob
  }
  # return(list(x = x, prob = p))
  return(x)
}