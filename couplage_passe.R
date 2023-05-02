# ---- Couplage dans le passe ----


# -- Fonction effectuant K itérations de couplage 
# --- Elle renvoie :
# res : booolen TRUE si les chaines ont coalesce en K iterations
# x : etat de la chaine x apres K iterations ( utile seulement si res = TRUE)
# it : l'iteration a laquelle elles ont coalesce 

coupl_mult <- function(x,y,K,p,N){
  res<- FALSE
  it <- 0
  for (i in seq_len(K)){
    a <- coupl_step(x,y,p,N)
    x <- a$x
    y <- a$y
    if (identical(x,y) & !res){
      res <- TRUE
      it <- i
    }
    
  }
  return(list(res = res, x = x, it = it))
}


# --- Fonction qui effectue la methode de couplage dans le passe ---
# --- Elle effectue K iterations de couplage, si les chaines n'ont pas coalesce, on recommence avec K + lag iterations

couplage_passe <- function(x,y,p,N, M, lag=100){
  K<- round(estim_K(x, y, p, N, M)$estim) # Estimation de K sur M ponts en fonction N 
  test <- coupl_mult(x,y,K,p,N)
  while (!test$res){
    K = K + lag
    test <- coupl_mult(x,y,K,p,N)
  }
  return(list(x = test$x, it = test$it, K = K))
}

couplage_passe(enveloppe(6,TRUE), enveloppe(6, FALSE), 1/2, 6, 200)

#Matrice de M pont resultant d'un couplage from the past
matrix_couplage_passe <- function(x0, y0, M, p, N) {
  ans <- matrix(0, M, N)
  attente <- numeric(M)
  for (i in seq_len(M)) {
    k <- couplage_passe(x0,y0,p,N,M)
    attente[i] <- k$K - k$it
    ans[i,] <-diff(k$x)
  }
  return(list( ans = ans, attente = attente)) # attente correspond au temps de melange apres coalescence
}
matrix_couplage_passe(enveloppe(6,TRUE), enveloppe(6, FALSE), 200, 1/2, 6)
