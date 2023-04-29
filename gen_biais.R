#---Generateur biaise---
#--Force passage dans F a partir d'un element de E--

gen_biais <- function(N) {
  x <- sample(c(-1, 1), N, prob = c(1 / 2, 1 / 2), replace = T)
  nb_1 <- sum(x == 1)
  if (nb_1 > N / 2) {
    indi <- which(x == 1)[1:(nb_1 - N / 2)]
    x[indi] <- -1
    return(x)
  }
  if (nb_1 < N / 2) {
    indi <- which(x == -1)[1:((N - nb_1) - N / 2)]
    x[indi] <- 1
    return(x)
  }
  return(x)
}


matrix_gen_biais <- function(M, N) {
  ans <- matrix(0, M, N)
  for (i in seq_len(M)) {
    x <- sample(c(-1, 1), N, prob = c(1 / 2, 1 / 2), replace = T)
    nb_1 <- sum(x == 1)
    if (nb_1 > N / 2) {
      indi <- which(x == 1)[1:(nb_1 - N / 2)]
      x[indi] <- -1
    }
    else if (nb_1 < N / 2) {
      indi <- which(x == -1)[1:((N - nb_1) - N / 2)]
      x[indi] <- 1
    }
    ans[i,] <- x
  }
  return(ans)
}

