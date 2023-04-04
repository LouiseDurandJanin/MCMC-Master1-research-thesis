#source("D:/M1/MemoireM1/Fonctions_de_base.R")
source("/Users/louisedurand-janin/Desktop/Mémoire/Ghithub/proba_accept_rejet.r")
#source("/Users/elsaazoulay/OneDrive - Université Paris-Dauphine/Dauphine M1/Mémoire/Fonctions_de_base.r")

#--Generateur de M ponts de taille N par la methode du rejet----
#-----Methode intelligente :on simule que M*(1/m_rejet) suivant l'uniforme
#-----sur E pour en moyenne passer 1 fois dans while"



rejet_optim <- function(M, N) {
  ans <- c()
  p_accept <- ceiling(proba_accept_rejet(N))
  left <- M
  while (left > 0) {
    x <- matrix(sample(c(-1, 1), N * left * p_accept, replace = TRUE), nrow = left * p_accept)
    ans <- rbind(ans, x[rowSums(x) == 0, ])
    left <- M - nrow(ans)
  }
  return(ans)
}
