
proba_accept_rejet <- function(N){
  return(1/(((2**N) * factorial(N/2)**2)/factorial(N)))
}