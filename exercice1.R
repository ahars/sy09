# UV : SY09 - TP04 : Analyses discriminante quadratique et linéaire
# Exercice 1 : Règle de Bayes
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice1.R

library(MASS)

# 1. - Donner une équation de la frontière de décision dans chacun des 5 cas à étudier.

# 2.
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

n = 1000

pi1 = 0.5
pi2 = 0.1
pi3 = 0.6

mu1 = matrix(c(0, 0), 2, 1)
mu2 = matrix(c(1, 1), 2, 1)

si1 = matrix(c(1, 0, 0, 1), 2, 2)
si2 = matrix(c(1, -0.3, -0.3, 1), 2, 2)
si3 = matrix(c(5, 0, 0, 5), 2, 2)
si4 = matrix(c(1, 0.5, 0.5, 1), 2, 2)

e1 = simul(n, pi1, mu1, mu2, si1, si1)
e2 = simul(n, pi2, mu1, mu2, si1, si1)
e3 = simul(n, pi1, mu1, mu2, si2, si2)
e4 = simul(n, pi3, mu2, mu2, si1, si3)
e5 = simul(n, pi3, mu1, mu2, si1, si4)

png(file = "plots/exo1_simul_1.png")
plot(e1[e1[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(e1[e1[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "population 1, n = 1000")
abline(lty = 1, a = 1, b = -1, col = 1)
dev.off()

png(file = "plots/exo1_simul_2.png")
plot(e2[e2[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(e2[e2[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "population 2, n = 1000")
abline(lty = 1, a = -1.197, b = -1, col = 1)
dev.off()

png(file = "plots/exo1_simul_3.png")
plot(e3[e3[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(e3[e3[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "population 3, n = 1000")
abline(lty = 1, a = 1, b = -1, col = 1)
dev.off()

png(file = "plots/exo1_simul_4.png")
plot(e4[e4[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(e4[e4[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "population 4, n = 1000")
dev.off()

png(file = "plots/exo1_simul_5.png")
plot(e5[e5[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(e5[e5[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-3, 4), xlim = c(-3, 4), xlab = "", ylab = "", main = "population 5, n = 1000")
dev.off()


## nb: la frontière du c) est proche de celle du a)

calculMu <- function (ech) {

	mu = matrix(c(0, 0, 0, 0), 2, 2)
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
	
	mu[1] = mu[1] / tt1
	mu[2] = mu[2] / tt1
	mu[3] = mu[3] / tt2
	mu[4] = mu[4] / tt2
	
	return (mu)
}

regleEuclidienne <- function (x, mu1, mu2) {

	if ((abs(mu2["x"] - x[1])) > (abs(mu1["x"] - x[1]))) {
		return (0)
	} else {
		return (1)
	}
}

erreurEstimee <- function (ech, regle, mu1, mu2) {

	r = 0
	classement = apply(ech, 1, regle, mu1 = mu1, mu2 = mu2)

	for (i in 1:nrow(ech)) {

		if (classement[i] != ech[i, 3]) {
			r = r + 1
		}
	}

	r = r / nrow(ech)

	return (r)
}

est1 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
est2 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
est3 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
est4 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))
est5 = matrix(c(0, 0), 2, 2, dimnames = list(c("x", "y"), c("mu1", "mu2")))

est1 [, c(1, 2)] = calculMu(e1[1:(n * pi1), ])
est2 [, c(1, 2)] = calculMu(e2[1:(n * pi2), ])
est3 [, c(1, 2)] = calculMu(e3[1:(n * pi1), ])
est4 [, c(1, 2)] = calculMu(e4[1:(n * pi3), ])
est5 [, c(1, 2)] = calculMu(e5[1:(n * pi3), ])

proba1 = erreurEstimee(e1[(n * pi1 + 1):n, ], regleEuclidienne, est1[ ,"mu1"], est1[ ,"mu2"])
proba2 = erreurEstimee(e2[(n * pi2 + 1):n, ], regleEuclidienne, est2[ ,"mu1"], est2[ ,"mu2"])
proba3 = erreurEstimee(e3[(n * pi1 + 1):n, ], regleEuclidienne, est3[ ,"mu1"], est3[ ,"mu2"])
proba4 = erreurEstimee(e4[(n * pi3 + 1):n, ], regleEuclidienne, est4[ ,"mu1"], est4[ ,"mu2"])
proba5 = erreurEstimee(e5[(n * pi3 + 1):n, ], regleEuclidienne, est5[ ,"mu1"], est5[ ,"mu2"])

# Pour les trois premières situations, on ajoute le tracé de la frontière de décision.
# Pour chaque cas de figure, on donne l'expression d'un estimateur de la probabilité d'erreur, ainsi que sa réalisation sur l'échantillon correspondant.
# On compare avec la probabilité d'erreur théorique lorsqu'on sait la calculer.

