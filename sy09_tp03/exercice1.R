# UV : SY09 - TP03: Théorie de la décision
# Exercice 1 : Visualisation des données
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice1.R

library(MASS)

# 1)
# http://www.site-naheulbeuk.com/utbm/sy09/sy09_gauss.pdf
# http://www.tcts.fpms.ac.be/publications/phds/gosselin/GosselinB_Phd.pdf
# http://mrobache.nonutc.fr/site_utc/?section=UVs
# http://www.apmep.asso.fr/IMG/pdf/R_SimulEtProba1.pdf

# On veut générer des données pour simuler des problèmes.
# vecteurs aléatoires Ã  2 dimensions
# 2 gaussiennes: une moyenne et une variance.
# On génère 600 points au total tout en réglant la répartition dans les classes.
# sur la matrice, x en col1, y en col2 et son appartenance (1 ou 2)

# Probabilité d'etre dans la classe 1 ou 2
# Chaque essai -> dans quelle classe -> on choisit une probabilité
# En dessous ou au dessus de pi -> on remplit la derniere colonne (1 ou 2)

# Tirer aléatoirement un chiffre (0 et 1)

# dans le 1 on genere des donnees
# dans le 2 on va apprendre à retrouver une classe
# le classifieur va calculer la moyenne de chacune des classes, proche de celle générée en amont
# on a les classes, on a un ensemble de 300 points -> moy de chacune des 2 classes -> 2 moyennes au milieu du nuage
# la règle de decision -> regarder la distance entre les 2 moyennes et on attribue à la moyenne la plus proche
# voir ensuite si l'estimation est parfaite ou pas, si les 2 nuages de points se chevauchent -> apparition d'erreurs
# La variance indique à quel point on s'écarte lors que la distribution s'écarte

n = 600
pi = 1/2

mu1 = c(0, 0)
mu2 = c(10, 0)

si1 = matrix(c(1, 0, 0, 1), 2, 2)
si2 = matrix(c(1, 0, 0, 1), 2, 2)

si3 = matrix(c(1, 0, 0, 1), 2, 2)
si4 = matrix(c(6, 0, 0, 6), 2, 2)

si5 = matrix(c(1, 0, 0, 1), 2, 2)
si6 = matrix(c(9, 0, 0, 9), 2, 2)

si7 = matrix(c(5, 0, 0, 5), 2, 2)
si8 = matrix(c(5, 0, 0, 5), 2, 2)

si9 = matrix(c(10, 0, 0, 10), 2, 2)
si10 = matrix(c(10, 0, 0, 10), 2, 2)

# Fonction de création de l'échantillon.
simul <- function (n, pi, mu1, mu2, sigma1, sigma2) {

	# On crée la matrice contenant l'échantillon résultat de la fonction.
	result = matrix(nrow = n, ncol = 3)
	# Affectation de chaque élément de l'échantillon final à la classe 0 ou 1.
	for (k in 1:n) {
		rand = sample(0:1, 1)
		result[k, 3] = rand
		
		if (result[k, 3] == 0) {
			result[k, c(1, 2)] = mvrnorm(1, mu1, sigma1)
		} else {
			result[k, c(1, 2)] = mvrnorm(1, mu2, sigma2)
		}
	}
	return (result)
}

t1 = simul(n, pi, mu1, mu2, si1, si2)
t2 = simul(n, pi, mu1, mu2, si3, si4)
t3 = simul(n, pi, mu1, mu2, si5, si6)
t4 = simul(n, pi, mu1, mu2, si7, si8)
t5 = simul(n, pi, mu1, mu2, si9, si10)

png(file = "plots/plot_simul_1.png")
plot(t1[t1[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t1[t1[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 1, a2 = 1")
dev.off()

png(file = "plots/plot_simul_2.png")
plot(t2[t2[ ,3] == 0, c(1, 2)], col = c("blue"), ylim=c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t2[t2[ ,3] == 1, c(1, 2)], col = c("red"), ylim=c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 1, a2 = 6")
dev.off()

png(file = "plots/plot_simul_3.png")
plot(t3[t3[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t3[t3[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 1, a2 = 9")
dev.off()

png(file = "plots/plot_simul_4.png")
plot(t4[t4[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t4[t4[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 5, a2 = 5")
dev.off()

png(file = "plots/plot_simul_5.png")
plot(t5[t5[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t5[t5[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 10, a2 = 10")
dev.off()

# 2)
# Calcul des estimateurs des moyennes mu1 et mu2 sur l'échantillon d'apprentissage.
calculMu <- function (ech) {

	mu = matrix(c(0, 0, 0, 0), 2, 2) # lignes puis colonnes.
	tt1 = 0
	tt2 = 0

	for (i in 1:nrow(ech[1:(nrow(ech)), ])) {
		if (ech[i,3] == 0) {
			mu[1] = mu[1] + ech[i,1]
			mu[2] = mu[2] + ech[i,2]
			tt1 = tt1 + 1
		} else {
			mu[3] = mu[3] + ech[i,1]
			mu[4] = mu[4] + ech[i,2]
			tt2 = tt2 + 1
		}
	}
	
	# Calcul des moyennes empiriques 1 et 2.
	mu[1] = mu[1] / tt1
	mu[2] = mu[2] / tt1
	mu[3] = mu[3] / tt2
	mu[4] = mu[4] / tt2
		
	return (mu)
}

et1 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
et2 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
et3 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
et4 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
et5 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))

et1[, c(1, 2)] = calculMu(t1[1:(n * pi), ])
et2[, c(1, 2)] = calculMu(t2[1:(n * pi), ])
et3[, c(1, 2)] = calculMu(t3[1:(n * pi), ])
et4[, c(1, 2)] = calculMu(t4[1:(n * pi), ])
et5[, c(1, 2)] = calculMu(t5[1:(n * pi), ])


# Fonction qui retourne la classe d'une observation x de l'échantillon du classifieur euclidien.
regleEuclidienne <- function (x, mu1, mu2) {

	if ((abs(mu2[1] - x[1])) > (abs(mu1[1] - x[1]))) {
		return (0)
	} else {
		return (1)
	}
}


# Fonction qui retourne la probabilité d'erreur estimée sur l'échantillon.
erreurEstimee <- function (ech, regle, mu1, mu2) {

	r = 0
	classement = apply(ech, 1, regle, mu1 = mu1, mu2 = mu2)
	
	for (i in 1:nrow(ech)) {
		if (classement[i] != ech[i, 3]) {
			r = r + 1
		}
	}
	r = r / nrow(ech) * 100
	return (r)
}

proba1 = erreurEstimee(t1[(n * pi + 1):n, ], regleEuclidienne, et1[ ,"mu1"], et1[ ,"mu2"])
proba2 = erreurEstimee(t2[(n * pi + 1):n, ], regleEuclidienne, et2[ ,"mu1"], et2[ ,"mu2"])
proba3 = erreurEstimee(t3[(n * pi + 1):n, ], regleEuclidienne, et3[ ,"mu1"], et3[ ,"mu2"])
proba4 = erreurEstimee(t4[(n * pi + 1):n, ], regleEuclidienne, et4[ ,"mu1"], et4[ ,"mu2"])
proba5 = erreurEstimee(t5[(n * pi + 1):n, ], regleEuclidienne, et5[ ,"mu1"], et5[ ,"mu2"])



# 3)
erreurMoyenne <- function (n, pi, mu1, mu2, sigma1, sigma2, regle, intervalle) {

	result = matrix(c(0, 0, 0, 0), 1, 4)

	for (i in 1:10) {
	
		# Création de l'échantillon.
		ech = simul(n, pi, mu1, mu2, sigma1, sigma2)
		
		# Calcul des estimateurs des moyennes mu1 et mu2
		esti = calculMu(ech[1:(n * pi), ])
		
		# Calcul de la probabilité d'une erreur.
		e = erreurEstimee(ech[(n * pi + 1):n, ], regle, esti[ ,1], esti[ ,2]) / 100
		result[1] = result[1] + e
		result[2] = e^2 / 10
	}

	# Moyenne
	result[1] = result[1] / 10
	
	# Variance
	result[2] = result[2] - result[1]^2
	
	# Écart-type
	e = 10 / (10 - 1) * result[2]
	
	# Intervalle de Confiance de niveau 5%
	result[3] = result[1] - intervalle * e / sqrt(10)
	result[4] = result[1] + intervalle * e / sqrt(10)

	return (result)
}

intervalle = 0.05
p1 = erreurMoyenne(n, pi, mu1, mu2, si1, si2, regleEuclidienne, intervalle)
p2 = erreurMoyenne(n, pi, mu1, mu2, si3, si4, regleEuclidienne, intervalle)
p3 = erreurMoyenne(n, pi, mu1, mu2, si5, si6, regleEuclidienne, intervalle)
p4 = erreurMoyenne(n, pi, mu1, mu2, si7, si8, regleEuclidienne, intervalle)
p5 = erreurMoyenne(n, pi, mu1, mu2, si9, si10, regleEuclidienne, intervalle)






# ANNEXE
apply(t1[(n * pi + 1):n, ], 1, regleEuclidienne, mu1 = et1[ ,"mu1"], mu2 = et1[ ,"mu2"])
t1[(n * pi + 1):n, ]

# Moyennes sur l'ensemble des observations.
z1 = (sum(t1[ ,3]) / n) * 100
z2 = (1 - sum(t1[ ,3]) / n) * 100

# le nombre de lignes et de colonnes d'une matrice.
zz1 = nrow(t1)
zz2 = ncol(t1)



