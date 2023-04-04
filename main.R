source("proba_accept_rejet.R")


# --- Graphe de la probabilite d'acceptation du rejet en fonction de N --- #
plot(proba_accept_rejet(seq(2,100,2)),type="p",lwd=2, col="red",xlab="N",ylab="probabilité d'acceptation", main="Probabilité d'acceptation du rejet en fonction de N")

