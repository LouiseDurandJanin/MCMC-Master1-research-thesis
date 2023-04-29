# --- Fonction qui retourne le vecteur de proba uniforme en fonction de N --- 

mu <- function(N){
  cardF <- factorial(N) / (factorial(N / 2) ** 2)
  mu <- rep(1 / cardF, cardF)
  return(mu)
}