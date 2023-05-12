# ---- Couplage dans le passe ----
source("vect_estim.R")

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

couplage_passe <- function(x, y, p, N, M, K_estim, lag=100){ 
  test <- coupl_mult(x, y, K_estim, p, N)
  while (!test$res){
    K_estim = K_estim + lag
    test <- coupl_mult(x, y, K_estim, p, N)
  }
  return(list(x = test$x, it = test$it, K = K_estim))
}

couplage_passe(enveloppe(6, TRUE), enveloppe(6, FALSE), 1/2, 6, 100, 100)


#Matrice de M pont resultant d'un couplage from the past
matrix_couplage_passe <- function(x0, y0, M, p, N) {
  ans <- matrix(0, M, N)
  K_estim <- estim_K(x0, y0, p, N, 100)$estim
  K <- numeric(M)
  for (i in seq_len(M)) {
    k <- couplage_passe(x0, y0, p, N, M, K_estim)
    K[i] <- k$K 
    ans[i,] <-diff(k$x)
  }
  return(list( ans = ans, K = K))
}


matrix_couplage_passe(enveloppe(6, TRUE), enveloppe(6, FALSE), 100, 1/2, 6)
