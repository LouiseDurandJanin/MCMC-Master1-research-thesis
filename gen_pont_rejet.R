#--Fonction qui génère un pont de taille N par la méthode du rejet--
#----Elle renvoie une liste contenant le pont et sa probabilité d'acceptation---

rejet <- function(N) {
  x <- sample(c(-1, 1), N, replace = TRUE)
  i <- 1
  while (sum(x) != 0) {
    x <- sample(c(-1, 1), N, replace = TRUE)
    i <- i + 1
  }
  return(list(x = x, prob = 1 / i))
}