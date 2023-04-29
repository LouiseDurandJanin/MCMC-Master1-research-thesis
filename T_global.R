# --- Operateur qui retourne les deux operateurs corner flip : T_u et T_d ---
# -- T_u : retourne un coin du bas vers le haut
# -- T_d : retourne un coin du haut vers le bas

T_global <- function(x, i) {
  y <- x
  if (x[i + 1]==x[i - 1]){
    if (x[i + 1] == (x[i] - 1)) {
      x[i] <- (x[i] - 2)
      return(list(T_u = y, T_d = x))
    }
    else{
      x[i] <- x[i] + 2
      return(list(T_u = x, T_d = y))
    }
  }
  else{
    return (list(T_u = x, T_d = y))
  }
}
