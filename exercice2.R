# UV : SY09 - TP04 : Analyses discriminante quadratique et linéaire
# Exercice 2 : Analyse discriminante sur les données Crabes
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice2.R

library(MASS)

# 2.

# Analyse discriminante linéaire.
couleur = c("blue", "red")
len = 50

crabs$FL1 = crabs$FL / (crabs$FL + crabs$RW + crabs$CL + crabs$CW + crabs$BD)
crabs$RW1 = crabs$RW / (crabs$FL + crabs$RW + crabs$CL + crabs$CW + crabs$BD)
D = crabs[ ,c(9, 10, 2)]


D.lda <- lda(D[ ,1:2], D$sex)
lx1p = seq(min(D$FL1), max(D$FL1), length = len)
lx2p = seq(min(D$RW1), max(D$RW1), length = len)

plot(D[ ,1:2], col = couleur[D$sex], pch = 20)
grille = data.frame(expand.grid(FL1 = lx1p, RW1 = lx2p))

ly = predict(D.lda, grille)
lyp = ly$post[ ,1] - ly$post[ ,2]
contour(lx1p, lx2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'green')


# Analyse discriminante quadratique.
D.qda <- qda(D[ ,1:2], D$sex)
qx1p = seq(min(D$FL1), max(D$FL1), length = len)
qx2p = seq(min(D$RW1), max(D$RW1), length = len)

plot(D[ ,1:2], col = couleur[D$sex], pch = 20)
grille = data.frame(expand.grid(FL1 = qx1p, RW1 = qx2p))

qy = predict(D.qda, grille)
qyp = qy$post[ ,1] - qy$post[ ,2]
contour(qx1p, qx2p, matrix(qyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'magenta')

# Calcul de l'erreur empirique lda sur l'ensemble d'apprentissage
n = dim(D)[1]
ly <- predict(D.lda, D[ ,1:2])
erreur.lda <- sum(ly$class != D$sex) / n

# Calcul de l'erreur empirique qda sur l'ensemble d'apprentissage
qy <- predict(D.qda, D[ ,1:2])
erreur.qda <- sum(qy$class != D$sex) / n


# 3.
# Fonction de création des échantillons d'apprentissage et de test (2/3 au hasard).
ech_crabs <- function (data, n) {

	rand = sample(0:2, n, replace = TRUE)
	
	for (k in 1:n) {
		if (rand[k] == 2) {
			rand[k] = 1
		}
	}

	result = cbind(data[ ,1:3], rand)
	return (result)
}
r = ech_crabs(D, n)



# 4.


# ANNEXE.
# crabs[1, ] donne la ligne 1
# et crabs[-1, ] nous donne la complémentaire de la ligne 1 (les lignes 2 à 200)

# point sur la fonction contour :
# tracer les courbes de niveau d'une fonction
# on a 2 classes en présence avec une fonction discriminante pour chacune des classes
# quand on veut représenter une courbe de niveau des ces fonctions :
# on peut les tracer sous R.
# On prend une grille de représentation (division de l'espace de représentation) -> espace en 2 dimensions
# pour chaque point de la grille, on calcule la valeur prise par la fonction
# la fonction contour va regarder les points qui sont autour d'une valeur donnée et va interpoler ces points pour les relier.
# Fonction surface pour faire la même chose en 3D.
