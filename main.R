source("gen_biaise.R")
source("gen_mat_rejet_naif.R")
source("gen_mat_rejet_optim.R")
source("gen_pont_rejet.R")
source("proba_accept_rejet.R")

# --- Graphe de la probabilite d'acceptation du rejet en fonction de N --- #
plot(proba_accept_rejet(seq(2,100,2)),type="p",lwd=2, col="red",xlab="N",ylab="probabilité d'acceptation", main="Probabilité d'acceptation du rejet en fonction de N")

