# --- Fonction qui calcule Q_p ---
# -- Q_p : matrice de transition de la recurrence aleatoire avec Ber(p) --
source("compt_coins.R")
source("T_global.R")

Q_p <- function(x, y, p, N) {
  C <- compt_coins(x)
  Cx_d <- C$cpt_d
  Cx_u <- C$cpt_u
  if (identical(x, y, num.eq = FALSE)) {
    return((N - 1 - (Cx_u + Cx_d) + (Cx_u * (1 - p)) + (Cx_d) * (p)) / (N - 1))
  }
  for (i in 2:N) {
    T_glob <- T_global(x, i)
    if (identical(y, T_glob$T_u, num.eq = FALSE)) {
      return((1 - p) / (N - 1))
    }
    else if (identical(y, T_glob$T_d, num.eq = FALSE)) {
      return(p / (N - 1))
    }
  }
  return(0)
}


