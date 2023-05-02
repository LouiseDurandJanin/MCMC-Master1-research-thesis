# --- Fonction renvoyant la probabilite d'acceptation de l'algo du rejet en fonction de la longueur N du pont ---

proba_accept_rejet <- function(N) {
  return(1 / (((2 ** N) * factorial(N / 2) ** 2) / factorial(N)))
}