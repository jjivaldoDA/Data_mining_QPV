 #-------------------------------------------------------------------------------
# 1) Les Statistiques descriptives 

library(tidyverse)
library(corrplot)


# La Matrice des corrélation 
cor <- cor(Etablissements[3:10])
corrplot(cor, method = "number")

#-------------------------------------------------------------------------------
# 3) L'analyse en composante Principal 
library(factoextra)
library(FactoMineR)

#ACP 
res2.pca <- PCA(Etablissements[,3:10])

# L'ensemble des resultats de l'ACP : 
summary(res2.pca)

#Cercle des corrélation + projection des individus
fviz_pca_var(res2.pca, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
fviz_pca_var(res2.pca, alpha.var = "contrib")

# Histogramme (des valeurs propres) en % de l'inertie):
round(res2.pca$eig,2)
fviz_screeplot (res2.pca, addlabels = TRUE, ylim = c(0, 60))

#-------------------------------------------------------------------------------
# 4) Classification : k-means + CAH
install.packages("fpc")
library(fpc)
install.packages("NbClust")
library(factoextra)
library(NbClust)

#Pour les K-MEANS ET CAH on supprime les variables COD GEO ET LIBGEO 
Etablissements$CODGEO <- NULL
Etablissements$LIBGEO <- NULL

#centrage et réduction des donnees
Etablissements.cr <- scale(Etablissements,center=T,scale=T)

#k-means avec les donnees centrees et reduites
#center = 4 - nombre de groupes repere sur le plan factoriel de l'ACP
groupes.kmeans <- kmeans(Etablissements.cr,centers=4,nstart=20)
print(groupes.kmeans)

#Determination du nb de classe optimal : methode du Coude
# methode du coude 
fviz_nbclust(Etablissements.cr, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Moyennes pour interpr?tation des classes 
#1) groupes 
groupe <- as.factor(groupes.kmeans$cluster)
#2) barycentres des classes 
centres <- NULL 
for (k in 1:4){
  ligne <- colMeans(Etablissements[groupe==k,1:8,drop=FALSE])
  centres <- rbind(centres,ligne)
}
numero <- seq(from=1, to=4)
rownames(centres) <- paste("clus_",numero,sep = "")
print(centres)

#CAH 
#Dendogramme 
res2.CAH=HCPC(res2.pca)
#Description des classes
res2.CAH$desc.var

#-------------------------------------------------------------------------------
# 5) l'arbre de décision

#Installation des packages 
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#Creation de la variable discretise 
Etablissements$Discretise <- ifelse(Etablissements$TX_TOT_EPREC > 13.209,1,0) 

#Arbre de decision globale 
globale=rpart(Discretise~TX_INDUS+TX_CONSTR+TX_COM_TRANSP+TX_COM_GROS+TX_COM_DETAIL+TX_SERV_ENT+TX_SERV_PAR, data=Etablissements, method="class")
rpart.plot(globale)


#Erreur de l'arbre de decision globale 
prediction = predict(globale, newdata = Etablissements, type = "class")
table = table(Etablissements$Discretise, prediction)
erreur = (table[1, 2] + table[2, 1]) / sum(table)
erreur


#Cr?ation du sous ?chantillon apprentissage et du sous-echantillon test
nb = nrow(Etablissements)
alea = 70
sample = sample(1:nb, alea)
apprentissage = Etablissements[sample, ]
test = Etablissements[-sample, ]

#Arbre de décision apprentissage  
arbre_apprentissage= rpart(Discretise~TX_INDUS+TX_CONSTR+TX_COM_TRANSP+TX_COM_GROS+TX_COM_DETAIL+TX_SERV_ENT+TX_SERV_PAR, data = apprentissage, method="class")
rpart.plot(arbre_apprentissage)

#Erreur methode Apprentissage-test

prediction_test= predict(arbre_apprentissage, newdata = test , type = "class")
table_erreur = table(test$Discretise, prediction_test)
erreur_test = (table_erreur[1, 2] + table_erreur[2, 1]) / sum(table_erreur)
erreur_test

#-------------------------------------------------------------------------------
# 6) Regression logistique 

install.packages("forestmodel")
library(questionr)
library(forestmodel)

Etablissements[, 1:9] <- sapply(Etablissements[, 1:9], as.numeric)


#1ere regression logistique 

logit <- glm( Discretise~TX_INDUS+TX_CONSTR+TX_COM_TRANSP+TX_COM_GROS+TX_COM_DETAIL+TX_SERV_ENT + TX_SERV_PAR, data=Etablissements, family=binomial(logit))
summary(logit)


# 2eme regression logisitque 

logit2 <- glm( Discretise~TX_INDUS+TX_CONSTR+TX_COM_TRANSP+TX_COM_GROS+TX_COM_DETAIL+TX_SERV_ENT, data=Etablissements, family=binomial(logit))
summary(logit2)

# TRANSFORMATION EN ODDS RATIO
odds.ratio(logit2)

# TABLEAU DES RESULTATS
forest_model(logit2)




