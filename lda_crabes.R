########################################################
########################################################
########################################################
library(MASS)
coul=c("black","magenta")

# Donnees "crabes"
# Utilisation des variables FL et RW pour reconnaître le sexe

######################################################################################
# Preparation des donnees 
crabs$FL1=crabs$FL/(crabs$FL+ crabs$RW+crabs$CL+crabs$CW+crabs$BD)
crabs$RW1=crabs$RW/(crabs$FL+ crabs$RW+crabs$CL+crabs$CW+crabs$BD)
D=crabs[,c(9,10,2)]
n=dim(D)[1]
plot(D[,1:2],col=coul[D$sex],pch=20)

######################################################################################
# Analyse discriminante lineaire, utilisant tout l'echantillon pour l'apprentissage 
D.lda <- lda(D[,1:2],D$sex)

# Visualisation des frontieres de decision
#   Choix d'une grille
len=50
x1p=seq(min(D$FL1),max(D$FL1),length=len)
x2p=seq(min(D$RW1),max(D$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))

#   Trace
y=predict(D.lda,grille)
yp=y$post[,1]-y$post[,2]
contour(x1p,x2p,matrix(yp,len),add=TRUE,levels=0,drawlabels=FALSE,col='green')

######################################################################################
# Calcul de l'erreur empirique  sur l'ensemble d'apprentissage
y <- predict(D.lda,D[,1:2])
erreur.lda <- sum(y$class != D$sex)/n

