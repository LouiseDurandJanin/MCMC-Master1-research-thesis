# --- Fonction qui compte le nombre de coins vers le haut et vers le bas ---

compt_coins <- function(x) {
  cpt_d <- 0
  cpt_u <- 0
  for (i in (2:(length(x) - 1))) {
    if (x[i - 1] == x[i + 1]) {
      if (x[i + 1] == (x[i] + 1)) {
      }
      cpt_d <- cpt_d + 1
    }
    else{
      cpt_u <- cpt_u + 1
    }
  }
  return(list(cpt_u = cpt_u, cpt_d = cpt_d))
}
