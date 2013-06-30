# UV : SY09 - TP02: Classification automatique
# Exercice 3 : Centres mobiles
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice3.R

# DONNEES IRIS
# 1)
# http://www.math-info.univ-paris5.fr/~jollois/site/documents/Classif_SID-Sante.pdf
# http://eric.univ-lyon2.fr/~bjouve/TP2.pdf

k1 = kmeans(iris[,1:4], 2, nstart = 20)
k2 = kmeans(iris[,1:4], 3, nstart = 20)
k3 = kmeans(iris[,1:4], 4, nstart = 20)

plot(iris[,1:4], col = c('red', 'blue')[k1$cluster])
plot(iris[,1:4], col = c('red', 'blue', 'green')[k2$cluster])
plot(iris[,1:4], col = c('red', 'blue', 'green', 'black')[k3$cluster])

# http://www.statmethods.net/advstats/cluster.html
library(cluster)
png(file = "plots/clusplot_kmeans_1.png")
clusplot(iris[,1:4], k1$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot Classification avec 2 classes")
dev.off()

png(file = "plots/clusplot_kmeans_2.png")
clusplot(iris[,1:4], k2$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot Classification avec 3 classes")
dev.off()

png(file = "plots/clusplot_kmeans_3.png")
clusplot(iris[,1:4], k3$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot Classification avec 4 classes")
dev.off()

# 2)
png(file = "plots/clusplot_kmeans_4.png")
clusplot(iris[,1:4], k2$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot Classification avec 3 classes")
dev.off()

# 3)
# faire une liste pour stocker le kmeans
# init 9 listes
# for jusque 100

# a)
# http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters


# b)
# Passer d'une liste à un numérique ou une matrice
z = as.numeric(unlist(r))
bar <- do.call(rbind, lapply(r, as.numeric))

# Le calcul du nombre de classes optimal
# La version 1X
l1 = list()
for (k in 2:10)
	l1[[k]] = sum(kmeans(iris[,1:4], k, nstart = 20)$withinss)
u1 <- do.call(rbind, lapply(l1, as.numeric))
plot(u1)

# La version 100X
l2 = list()
for (k in 2:10)
{
	l2[[k]] = list()
	for (x in 1:100)
		l2[[k]][[x]] = sum(kmeans(iris[,1:4], k, nstart = 20)$withinss)
}
r2 = list()
for (k in 2:10)
{
	z = as.numeric(unlist(l2[[k]]))
	r2[[k]] = sum(z) / 100
}
u2 <- do.call(rbind, lapply(r2, as.numeric))

png(file = "plots/plot_inertie_intra_1.png")
plot(u2, main = "Recherche du nombre de classes optimal", xlab = "Nombre de classes K", ylab = "Inertie intra-classe minimale", xlim = c(1, 10), type = "l")
dev.off();


# 4)
png(file = "plots/clusplot_kmeans_2.png")
clusplot(iris[,1:4], k2$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot Classification avec 3 classes")
dev.off()

png(file = "plots/clusplot_iris_10.png")
clusplot(iris[,1:4], iris$Species, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris avec les 3 espèces")
dev.off()




# DONNEES CRABS
# Nettoyage des données crabs
library(MASS)
crabsquant = crabs[,4:8]
crabsquant2 = crabsquant / crabsquant$CL
crabsquant2 = crabsquant2[,-3]

# Le calcul du nombre de classes optimal
l3 = list()
for (k in 2:13)
{
	l3[[k]] = list()
	for (x in 1:100)
		l3[[k]][[x]] = sum(kmeans(crabsquant2, k, nstart = 20)$withinss)
}
r3 = list()
for (k in 2:13)
{
	z = as.numeric(unlist(l3[[k]]))
	r3[[k]] = sum(z) / 100
}
u3 <- do.call(rbind, lapply(r3, as.numeric))

png(file = "plots/plot_inertie_intra_2.png")
plot(u3, main = "Recherche du nombre de classes optimal crabs", xlab = "Nombre de classes K", ylab = "Inertie intra-classe minimale", xlim = c(1, 12), type = "l")
dev.off();

km1 = kmeans(crabsquant2, 2, nstart = 20)
km2 = kmeans(crabsquant2, 3, nstart = 20)
km3 = kmeans(crabsquant2, 4, nstart = 20)

png(file = "plots/clusplot_crabs_10.png")
clusplot(crabsquant2, km1$cluster, color = TRUE, main = "Clusplot crabs (K = 2)", shade = TRUE, labels = 2)
dev.off();

png(file = "plots/clusplot_crabs_11.png")
clusplot(crabsquant2, km2$cluster, color = TRUE, main = "Clusplot crabs (K = 3)", shade = TRUE, labels = 2)
dev.off();


png(file = "plots/plot_crabs_sp_2.png")
plot(crabsquant2, col = crabs$sp, main = "Crabs - Représentation des espèces")
dev.off()

png(file = "plots/plot_crabs_sex_2.png")
plot(crabsquant2, col = crabs$sex, main = "Crabs - Représentation des sexes")
dev.off()


png(file = "plots/clusplot_crabs_12.png")
clusplot(crabsquant2, km3$cluster, color = TRUE, main = "Clusplot crabs (K = 4)", shade = TRUE, labels = 2)
dev.off();

png(file = "plots/clusplot_crabs_13.png")
clusplot(crabsquant2, crabs[,1:4], color = TRUE, shade = TRUE, labels = 2, main = "Clusplot crabs avec les espèces et les sexes")
dev.off()

cl <- pam(crabsquant2, 4)$clustering
clusplot(crabsquant2, cl, color = TRUE, main = "Clusplot iris, critère WARD", shade = TRUE, labels = 2)



