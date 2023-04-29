# --- Creer une enveloppe de taille N (sup est booleen) ---

enveloppe <- function(N, sup){
  if ( sup ){ return( c( 0 : ( N / 2 ), ( ( N / 2 - 1) : 0 ) ) ) }
  else { return( c( 0 : (- N / 2 ),( ( -N / 2 + 1) : 0) ) ) }
}