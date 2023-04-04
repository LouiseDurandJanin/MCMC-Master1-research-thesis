#---Generateur biaise---

f3 <- function(n) {
  x <- sample(c(-1, 1), n, prob = c(1 / 2, 1 / 2), replace = T)
  n1 <- sum(x == 1)
  if (n1 > n / 2) {
    indi <- which(x == 1)[1:(n1 - n / 2)]
    x[indi] <- -1
    return(x)
  }
  if (n1 < n / 2) {
    indi <- which(x == -1)[1:((n - n1) - n / 2)]
    x[indi] <- 1
    return(x)
  }
  return(x)
}


matrix_f3 <- function(m, n) {
  ans <- c()
  for (i in seq_len(m)) {
    ans <- rbind(ans, f3(n))
  }
  return(ans)
}

