rm(list = ls())
source("gen_biais.R")
source("matrix_rejet.R")
source("gen_pont_rejet.R")
source("proba_accept_rejet.R")
source("mu.R")
source("transfo_bin.R")
source("rec_mult.R")
source("matrix_rec_mult.R")
source("enveloppe.R")
source("vect_pvalue.R")
source("rec.R")
source("Q_p.R")
source("mh_sampler.R")
source("matrix_MH.R")
source("T_global.R")
source("compt_coins.R")
library(dplyr)
library(ggplot2)
# --- Graphe de la probabilite d'acceptation du rejet en fonction de N --- #



df <- data.frame(x = seq(2, 100, 2))
df <- df %>% mutate(y = proba_accept_rejet(x))
ggplot(df, aes(x, y)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  xlab ("N") +
  ylab("Probabilité d'acceptation") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size =
                                     14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size =
                                     14)) +
  theme_light()
ggsave(filename = "plot_proba_accept.png",
       scale = .6,
       path = "~/Desktop/Mémoire")



# --- Histogramme du generateur biaise ---


v_biais <- transfo_bin(matrix_gen_biais(100000, 6))
freq_biais <- table(v_biais)
freq_df_biais <-
  data.frame(valeur = names(freq_biais),
             frequence = as.numeric(freq_biais))

ggplot (freq_df_biais, aes(x = valeur, y = frequence)) +
  scale_x_discrete(limits = names(freq_biais)) +
  geom_col(fill = "#009999") +
  xlab ("Clés binaires") +
  ylab("Effectifs") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size = 14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size = 14)) +
  theme_light()

ggsave(filename = "hist_biais.png",
       scale = .6,
       path = "~/Desktop/Mémoire")

# --- P-valeur du test d'uniformité applique au generateur biaise ---
mu <- mu(6)
pvalue_biais <- chisq.test(table(v_biais), p = mu)$p.value

# --- Histogrammes de la recurrence aleatoire ---

mat_rec_200 <-
  matrix_rec_mult(10000, 6, 200, 1 / 2, enveloppe(6, TRUE))

# --- Histogramme apres 50 iterations ---

freq_rec_50 <-
  table(mat_rec_200[, 51]) #--On va jusqu'à 51 car la premiere cle stockee est l'initialisation
freq_df_50 <-
  data.frame(valeur = names(freq_rec_50),
             frequence = as.numeric(freq_rec_50))
ggplot (freq_df_50, aes(x = valeur, y = frequence)) +
  scale_x_discrete(limits = names(freq_rec_50)) +
  geom_col(fill = "#009999") +
  xlab ("Clés binaires") +
  ylab("Effectifs") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size =
                                     14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size =
                                     14)) +
  theme_light()


ggsave(filename = "hist_rec_50.png",
       scale = .6,
       path = "~/Desktop/Mémoire")

# --- Histogramme apres 100 iterations

freq_rec_100 <- table(mat_rec_200[, 101])
freq_df_100 <-
  data.frame(valeur = names(freq_rec_100),
             frequence = as.numeric(freq_rec_100))
ggplot (freq_df_100, aes(x = valeur, y = frequence)) +
  scale_x_discrete(limits = names(freq_rec_100)) +
  geom_col(fill = "#336999") +
  xlab ("Clés binaires") +
  ylab("Effectifs") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size =
                                     14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size =
                                     14)) +
  theme_light()


ggsave(filename = "hist_rec_100.png",
       scale = .6,
       path = "~/Desktop/Mémoire")


# --- Histogramme apres 200 iterations

freq_rec_200 <- table(mat_rec_200[, 201])
freq_df_200 <-
  data.frame(valeur = names(freq_rec_200),
             frequence = as.numeric(freq_rec_200))
ggplot (freq_df_200, aes(x = valeur, y = frequence)) +
  scale_x_discrete(limits = names(freq_rec_200)) +
  geom_col(fill = "#336666") +
  xlab ("Clés binaires") +
  ylab("Effectifs") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size =
                                     14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size =
                                     14)) +
  theme_light()


ggsave(filename = "hist_rec_200.png",
       scale = .6,
       path = "~/Desktop/Mémoire")


# --- Graphe d'evolution de la pvaleur en fonction du nombre d'iterations de la recurrence ---

vec_pvaleur <- vect_pvalue(1000, 6, 200, 1 / 2, enveloppe(6, TRUE))
df_pvaleur <- data.frame(x = seq(10, 200, by = 10), v = vec_pvaleur)

ggplot(df_pvaleur, aes(x, v)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  xlab ("Nombre d'itérations de la récurrence aléatoire") +
  ylab("P-valeur") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size =
                                     14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size =
                                     14)) +
  theme_light()
ggsave(filename = "plot_pvalue.png",
       scale = .5,
       path = "~/Desktop/Mémoire")

# --- Graphe d'autocorrelation pour les 200 premieres iterations de la recurrence avec p=1/2 ---
vec_1000 <- transfo_bin(rec_mult(1000, enveloppe(6, TRUE), 6, 1 / 2))
#df_acf <- data.frame( x=seq(0, 500), y=vec_500)
#ggplot(df_acf, aes(x, y)) +
geom_col(fill = "red") +
  xlab ("Nombre d'itérations de la récurrence aléatoire") +
  ylab("P-valeur") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(face = "bold",
                                   size =
                                     14)) +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(face = "bold",
                                   size =
                                     14)) +
  theme_light()
plot(
  acf(vec_1000, plot = FALSE, lag.max = 1000),
  main = " p = 1/2",
  xlab = "Itérations de la récurrence",
  ylab = "Corrélation entre deux itérations successives"
)

# --- Graphe d'autocorrelation pour les 200 premieres iterations de MH avec p=1/6---
vec_MH_1000 <-
  acf(transfo_bin(mh_sampler(enveloppe(6, TRUE), 1000, 1 / 6, 6)),
      plot = FALSE,
      lag.max = 1000)
#df_acf_MH <- data.frame( x=seq(0, 500), y=vec_500)

plot(vec_MH_1000,
     main = "p=1/6",
     xlab = "Itérations de la récurrence",
     ylab = "Corrélation entre deux itérations successives")
