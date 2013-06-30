# UV : SY09 - TP02: Classification automatique
# Exercice 1 : Visualisation des données
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice1.R

# 1
library(MASS)
library(ade4)

data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$cls <- iris[,5]

# Schéma 1 : axe factoriel
acp1 <- princomp(donnees$num)
png(file = "plots/biplot_axe_factoriel_1.png")
biplot(acp1, main = "Premier axe factoriel", xlab = "axe 1", ylab = "axe 2", xlim = c(-0.2, 0.25))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

# Schéma 2 (facultatif) : axe factoriel
# Il veut pas s'enregistrer dans un fichier.... ><
acp2 = dudi.pca(donnees$num)
png(file = "plots/corcircle_axe_factoriel_2.png")
s.corcircle(b$co, xax = 1, yax = 2, sub = "Premier plan factoriel", box = TRUE)
dev.off()

# On constate que les variables petal.width et petal.length sont 
# fortement corrélées (ce qui semble logique car il s'agit de tailles)
# Ce n'est en revanche pas le cas pour les variables sepal.width et
# sepal.length qui forment un angle de près de 90°
# (see http://www.radisma.info/docannexe.php?id=522)

# 2
library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
crabsquant <- crabsquant / matrix(rep(crabsquant[,4], dim(crabsquant)[2]), nrow = dim(crabsquant)[1], byrow = F)
crabsquant <- crabsquant[,-4]

# ACP des données
acp3 <- princomp(crabsquant)
png(file = "plots/biplot_axe_factoriel_3.png")
biplot(acp3, main = "Graphe de l'ACP de Crabsquant après traitement de l'effet taille")
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

# plot des variables RW et BD
png(file = "plots/plot_crabsquant_bd_rw.png")
plot(crabsquant$RW, crabsquant$BD, xlab = "RW", ylab = "BD", main = "Représentation des données RW et BD de Crabsquant")
dev.off()

# Sur le biplot, on constate que les variables RW et BD forment un angle droit.
# Les nuages de points du biplot et celui du plot des variables RW et BD
# sont de formes très semblables

# 3

# pourquoi AFTD et pas ACP ? => les données mutations sont données sous la forme
# d'une matrice de distances (complètement différent de crabs ou iris) entre espèces
# (distance: nombre de mutations d'une certaine protéine)
# l'ACP n'a aucun sens dans ce cas là
# Idée générale de l'AFTD: chercher le tableau individu / variable qui pourrait donner le 
# tableau de distances original <=> vecteurs tel que distance euclidienne entre 2 vecteurs
# = distance originale entre deux espèces.
# cmdscale: param k défini la taille de l'espace dans lequel je définis les vecteurs
# représentant les individus, par défaut 2.
# Shepard: représentation servant à évaluer la qualité de l'AFTD:
# un axe plote les distances originales et l'autre les distances
# reproduites par l'AFTD. Le diagramme de Shepard sert à mesurer l'exactitude
# entre les distances originales et les distances retrouvées par l'AFTD.
# "Si tout est parfait, tous les points sont sur une diagonale" (pas sûr)

# nb: nécessité d'utiliser lower.tri ou as.dist pour obtenir une matrice triangulaire

setwd("C:/Users/sy09p039/Desktop/sy09/td02")
mutations <- read.table("mutations2.txt", header = F, row.names = 1)

png(file = "plots/plot_shepard_1.png")
s1 = Shepard(as.dist(mutations), cmdscale(mutations, k = 2))
plot(s1, main = "diagramme de Shepard de Mutations (k = 2)", pch = "*")
abline(0, 1)
dev.off();

png(file = "plots/plot_shepard_2.png")
plot(Shepard(as.dist(mutations), cmdscale(mutations, k = 14)), main = "diagramme de Shepard de Mutations (k = 14)")
abline(0, 1)
dev.off();

