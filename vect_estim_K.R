# --- Fonction qui renvoie la valeur estimée de K temps de couplage 
source("couplage.R")
source("enveloppe.R")

estim_K <- function(x, y, p, N, M){
  K <- rep(0, M)
  for (i in seq_len(M)){
    ans <- couplage(x,y, p, N)
    K[i] <- ans$cpt
  }
  return(list(estim = mean(K), max = max(K), var = var(K)))
}

# --- Fonction renvoyant un vecteur de valeurs estimées de K associées au longueur de chaîne dans un ecteur x

vect_estim_K <- function(N_max, M){
  v <- c()
  x <- seq(4, N_max, by=2)
  for (i in x){
    K <- estim_K(enveloppe(i,TRUE), enveloppe(i,FALSE), 1/2, i, M)
    v <- c(v, K$estim)
  }
  return(v)
}

