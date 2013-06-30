# UV : SY09 - TP03: Théorie de la décision
# Exercice 2 : Règle de Bayes
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice2.R

library(MASS)

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

# 1)
pi = 1 / 2
n1 = 10
n2 = 100
n3 = 1000
n4 = 10000
n5 = 100000

mu1 = c(-1, 0)
si1 = matrix(c(1, 0, 0, 1), 2, 2)
mu2 = c(1, 0)
si2 = matrix(c(1, 0, 0, 1), 2, 2)

t6 = simul(n1, pi, mu1, mu2, si1, si2)
t7 = simul(n2, pi, mu1, mu2, si1, si2)
t8 = simul(n3, pi, mu1, mu2, si1, si2)
t9 = simul(n4, pi, mu1, mu2, si1, si2)
t10 = simul(n5, pi, mu1, mu2, si1, si2)

png(file = "plots/plot_simul_6.png")
plot(t6[t6[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t6[t6[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 10")
dev.off()

png(file = "plots/plot_simul_7.png")
plot(t7[t7[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t7[t7[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 100")
dev.off()

png(file = "plots/plot_simul_8.png")
plot(t8[t8[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t8[t8[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 1 000")
dev.off()

png(file = "plots/plot_simul_9.png")
plot(t9[t9[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t9[t9[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 10 000")
dev.off()

png(file = "plots/plot_simul_10.png")
plot(t10[t10[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t10[t10[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 100 000")
dev.off()

# 2)
var(t6[ ,c(1, 2)][,3 == 0])
var(t7[ ,c(1, 2)])
var(t8[ ,c(1, 2)])
var(t9[ ,c(1, 2)])
var(t10[ ,c(1, 2)])

# Courbes d'isodensité
png(file = "plots/plot_isodensite.png")
plot(t8[t8[ ,3] == 0, c(1, 2)], col = c("green"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t8[t8[ ,3] == 1, c(1, 2)], col = c("yellow"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "Courbes d'isodensité, n = 1 000")
symbols(1, 0, circles = 0.671, inches = F, add = T)
symbols(-1, 0, circles = 0.671, inches = F, add = T)
dev.off()

png(file = "plots/plot_frontiere_1.png")
plot(t8[t8[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t8[t8[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 1 000, frontière 1")
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

png(file = "plots/plot_frontiere_2.png")
plot(t8[t8[ ,3] == 0, c(1, 2)], col = c("blue"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(t8[t8[ ,3] == 1, c(1, 2)], col = c("red"), ylim = c(-5, 5), xlim = c(-5, 5), xlab = "", ylab = "", main = "n = 1 000, frontière 2 et 3")
abline(lty = 1, a = 1000000, b = 1000000, col = 1)
dev.off()





# ANNEXE
calculEstimations <- function (ech) {

	result = matrix(c(0, 0, 0, 0), 2, 4) # lignes puis colonnes.
	tt1 = 0
	tt2 = 0

	for (i in 1:nrow(ech[1:(nrow(ech)), ])) {
		if (ech[i, 3] == 0) {
			result[1, 1] = result[1,1] + ech[i,1]
			result[2, 1] = result[2,1] + ech[i,2]
				
			result[1, 2] = ech[i,1]^2 / nrow(ech)
			result[2, 2] = ech[i,2]^2 / nrow(ech)
			
			tt1 = tt1 + 1
		} else {
			result[1, 3] = result[1, 3] + ech[i,1]
			result[2, 3] = result[2, 3] + ech[i,2]

			result[1, 4] = ech[i,1]^2
			result[2, 4] = ech[i,2]^2

			tt2 = tt2 + 1
		}
	}
	result[1, 1] = result[1, 1] / tt1
	result[2, 1] = result[2, 1] / tt1

	result[1, 3] = result[1, 3] / tt1
	result[2, 3] = result[2, 3] / tt1

	result[1, 2] = (result[1, 2] - result[1, 1]^2) / tt1
	result[2, 2] = (result[2, 2] - result[2, 1]^2) / tt1
	
	result[1, 4] = (result[1, 4] - result[1, 3]^2) / tt2
	result[2, 4] = (result[2, 4] - result[2, 3]^2) / tt2
	
	return (result)
}




# Le rayon est corrélé avec l'écart type.
# écart type faible, gaussienne serrée.

# intervalle de confiance avec des tests de Student.

# 3)

# 4)
# a)

# b)
# Donner une estimation avec 5 exemples, avec 10 exemples dans la 2)
# i.

# ii.

# iii.

# c)
