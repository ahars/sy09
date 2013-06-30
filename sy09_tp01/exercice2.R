# UV : SY09 - TP01
# 1) Statistique descriptive
# Exercice 2 : Données crabs
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice2.R

# 1.1
png(file = "plots/boxplot_fl_espece.png")
boxplot(crabs$FL[crabs$sp == "B"], crabs$FL[crabs$sp == "O"], main = "Étude de la variable FL en fonction des espèces",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_rw_espece.png")
boxplot(crabs$RW[crabs$sp == "B"], crabs$RW[crabs$sp == "O"], main = "Étude de la variable RW en fonction des espèces",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_cl_espece.png")
boxplot(crabs$CL[crabs$sp == "B"], crabs$CL[crabs$sp == "O"], main = "Étude de la variable CL en fonction des espèces",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_cw_espece.png")
boxplot(crabs$CW[crabs$sp == "B"], crabs$CW[crabs$sp == "O"], main = "Étude de la variable CW en fonction des espèces",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_bd_espece.png")
boxplot(crabs$BD[crabs$sp == "B"], crabs$BD[crabs$sp == "O"], main = "Étude de la variable BD en fonction des espèces",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_fl_sexe.png")
boxplot(crabs$FL[crabs$sex == "M"], crabs$FL[crabs$sex == "F"], main = "Étude de la variable FL en fonction du sexe",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_rw_sexe.png")
boxplot(crabs$RW[crabs$sex == "M"], crabs$RW[crabs$sex == "F"], main = "Étude de la variable RW en fonction du sexe",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_cl_sexe.png")
boxplot(crabs$CL[crabs$sex == "M"], crabs$CL[crabs$sex == "F"], main = "Étude de la variable CL en fonction du sexe",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_cw_sexe.png")
boxplot(crabs$CW[crabs$sex == "M"], crabs$CW[crabs$sex == "F"], main = "Étude de la variable CW en fonction du sexe",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "plots/boxplot_bd_sexe.png")
boxplot(crabs$BD[crabs$sex == "M"], crabs$BD[crabs$sex == "F"], main = "Étude de la variable BD en fonction du sexe",  col = c("blue", "red"), names = c("B", "O"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()



# 1.2
library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]

png(file = "plots/boxplot_crabsquant.png")
boxplot(crabsquant, main = "les données de crabsquant", ylab = "nombre de crabes", notch = TRUE)
dev.off()

png(file = "plots/plot_crabsquant_sp.png")
plot(crabsquant, col = crabs$sp)
# plot(crabsquant,col=c("red","blue")[crabs$sp])
dev.off()

png(file = "plots/plot_crabsquant_sex.png")
plot(crabsquant, col = crabs$sex)
# plot(crabsquant,col=c("red","blue")[crabs$sex])
dev.off()

# Ne permet pas de voir déterminer une espèce en fonction d'une variable de manière précise.

# On peut voir qu'il y a une corrélation forte entre toutes les combinaisons de variables
# Cause de la forte corrélation entre les variables ?
# Il s'agit de mesures de certaines parties du corps des crabes. Par conséquent,
# il est donc normal que chaque variable soit proportionnelle aux autres. (le contraire signifierait
# que tous les crabes n'ont pas de proportions corporelles harmonieuses).

# Calcul de la corrélation entre les variables
cor(crabsquant)



# 2.1
m = matrix(c(3,4,3,1,4,3,2,3,6,2,1,4),nrow = 4, byrow = T)

# Centrage de la matrice de base en soustrayant à chaque colonne la moyenne correspondante
# cent = sweep(m, 2, colMeans(m))
cent = scale(m, center = TRUE, scale = FALSE)

# Calcul de la matrice de variance : (1/n).Y.Y' ce qui nous donne a
# n = 4 ici, Y = cent et Y' = t(cent)
vari = (1/4) * t(cent) %*% cent
# %*% est utile pour faire un produit mathématique de 2 matrices.

# Diagonalisation de la matrice pour obtenir les valeurs propres et vecteurs propres.
diag = eigen(vari)
# diag$values -> le vecteur des valeurs propres -> ici 2.0, 1.0, 0.5
# diag$vectors -> la matrice des vecteurs propres

# Calcul du pourcentage des axes d'inertie
# a = 2.0 + 1.0 + 0.5
# axe1 = 2.0 / a * 100
# axe2 = 1.0 / a * 100
# axe3 = 0.5 / a * 100

# Calcul du pourcentage des axes d'inertie cumulés
# axec1 = 2.0 / a * 100
# axec2 = 1.0 / a * 100 + axec1
# axec3 = 0.5 / a * 100 + axec2

# Calcul de la matrice des composantes principales
comp = cent %*% diag$vectors

acpp = princomp(m)
png(file = "plots/plot_composantes_principales.png")
biplot(acpp, main = "Représentation des individus dans le premier plan factoriel", ylab = "axe 2", xlab = "axe 1")
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

# Calcul des corrélations entre les variables
corr = cor(cent, comp)
png(file = "plots/plot_correlation.png")
plot(corr, main = "représentation des 3 variables dans le 1er plan factoriel", xlab = "axe 1", ylab = "axe 2", xlim = c(-1, 1), ylim = c(-1, 1), col = "red")
text(corr, pos = 1, labels = c("1", "2", "3"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.965, inches = F, add = T)
dev.off()



# 2.2
notes = matrix(c(6.0, 6.0, 5.0, 5.5, 8.0, 8.0, 8.0, 8.0, 8.0, 9.0, 6.0, 7.0, 11.0, 9.5, 11.0, 14.5, 14.5, 15.5, 15.0, 8.0, 14.0, 14.0, 12.0, 12.5, 10.0, 11.0, 10.0, 5.5, 7.0, 13.0, 5.5, 7.0, 14.0, 11.5, 10.0, 13.0, 12.5, 8.5, 9.5, 12.0, 9.0, 9.5, 12.5, 12.0, 18.0), nrow = 9, byrow = T)
colnames(notes) = c("math", "scie", "fran", "lati", "d-m")
rownames(notes) = c("jean", "aline", "annie", "monique", "didier", "andré", "pierre", "brigitte", "evelyne")

# Centrage de la matrice de base en soustrayant à chaque colonne la moyenne correspondante
centrage = scale(notes, center = TRUE, scale = FALSE)

# Calcul de la matrice de variance
variance = (1/9) * t(centrage) %*% centrage

# Diagonalisation de la matrice pour obtenir les valeurs propres et vecteurs propres.
diago = eigen(variance)

# Calcul de la matrice des composantes principales
mcp = centrage %*% diago$vectors

png(file = "plots/plot_mcp.png")
plot(mcp, xlim = c(-12, 10), ylim = c(-7, 6), col = "red")
text(mcp, pos = 1, labels = c("jean", "aline", "annie", "monique", "didier", "andré", "pierre", "brigitte", "evelyne"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

# ACP des notes

acp = princomp(notes)
png(file = "plots/plot_acp_notes.png")
plot(acp$scores, main = "Représentation des individus sur les axes 1 et 2", xlab = "axe 1", ylab = "axe 2", col = "red")
text(acp$scores, pos = 1, labels = c("jean", "aline", "annie", "monique", "didier", "andré", "pierre", "brigitte", "evelyne"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

# Calcul des corrélations entre les variables
corre = cor(centrage, acp$scores)
png(file = "plots/plot_correlation_notes.png")
plot(corre, main = "Représentation des 3 variables dans le 1er plan factoriel", xlab = "axe 1", ylab = "axe 2", xlim = c(-1, 1), ylim = c(-1, 1), col = "red")
text(corre, pos = 1, labels = c("math", "scie", "fran", "lati", "d-m"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.965, inches = F, add = T)
dev.off()

png(file = "plots/plot_correlation_notes_2.png")
plot(cbind(corre[,1], corre[,3]), main = "Représentation des 3 variables dans le 2eme plan factoriel", xlab = "axe 1", ylab = "axe 3", xlim = c(-1, 1), ylim = c(-1, 1), col = "red")
text(cbind(corre[,1], corre[,3]), pos = 1, labels = c("math", "scie", "fran", "lati", "d-m"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()



# 2.3
library(MASS)
crabsquant = crabs[,4:8]
acp2 = princomp(crabsquant)

png(file = "plots/plot_acp2_crabs.png")
plot(acp2, main = "")
dev.off()

png(file = "plots/biplot_acp2_crabs.png")
biplot(acp2, main = "Représentation de la corrélation sans traitement")
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

crabsquant2 = crabsquant / crabsquant$CL
crabsquant2 = crabsquant2[,-3]
acp3 = princomp(crabsquant2)

png(file = "plots/plot_acp3_crabs.png")
plot(acp3, main = "")
dev.off()

png(file = "plots/biplot_acp3_crabs.png")
biplot(acp3, main = "Représentation de la corrélation après traitement")
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

png(file = "plots/plot_crabs_sp_2.png")
plot(crabsquant2, col = crabs$sp)
dev.off()

png(file = "plots/plot_crabs_sex_2.png")
plot(crabsquant2, col = crabs$sex)
dev.off()



#spb1 = crabs$FL[crabs$sp == 'B'] # Liste des valeurs de FL pour l'espèce B
#spo1 = crabs$FL[crabs$sp == 'O'] # Liste des valeurs de FL pour l'espèce O

# histogramme de la variable FL divisée en classes pour l'espèce B
#hist1 = hist(spb1, col=rgb(0,0,1,1/2), breaks=c(5,10,15,20,25), labels=c("5-10","10-15","15-20","20-25"), xlab="FL", main="FL pour l'espèce B")
# histogramme de la variable FL divisée en classes pour l'espèce O
#hist2 = hist(spo1, col=rgb(1,0,1,1/2), breaks=c(5,10,15,20,25), labels=c("5-10","10-15","15-20","20-25"), xlab="FL", main="FL pour l'espèce O")

# histogramme des effectifs "côte à côte"
#x11()
#barplot(rbind(hist1$counts, hist2$counts), col = c("blue","red"), beside = T, main = "Étude de la variable FL en fonction des espèces", legend.text = T, ylab = "nombre de crabes")

# NB: À faire pour chaque variable en fonction du sexe et de l'espèce (x10 histogrammes)

#barplot(rbind(NonSmoke, Smoke), space = 0,legend = levels('Smoke'), main = "Comparaison du niveau d'étude des mères", col = c('blue','red'), xlab = "niveaux d'étude", ylab = "nombre de femmes", legend.text = T)

