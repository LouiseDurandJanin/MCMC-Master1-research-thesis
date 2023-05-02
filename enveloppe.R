# --- Creer une enveloppe de taille N ---
# --- Sup est booleen : si TRUE : renvoie enveloppe superieure ---

enveloppe <- function(N, sup) {
  if (sup) {
    return(c(0:(N / 2), ((N / 2 - 1):0)))
  }
  else {
    return(c(0:(-N / 2), ((-N / 2 + 1):0)))
  }
}