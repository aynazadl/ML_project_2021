#Réseau de neurone linéaire Optimal
library(rpart)
library(rpart.plot)
library(nnet)

#creation des données d'apprentissage
data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:24)])
names(data.acp)[1] = "Target_without_mask"

#Echantillonage
sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
train= data.acp[sample, ] #75% de lignes
test= data.acp[-sample, ] #25% de lignes

#Application de nnet linéaire 
res.lnnetO = nnet(Target_without_mask~., data=train ,size=0,
                 skip=TRUE,rang=0.1,linout=TRUE,
                 decay=5e-4,maxit=2000,MaxNWts=3500)

#prédiction
pred.lnnetO=predict(res.lnnetO, test, type="class")
#Taux de mal classé
Tmc.lnnetO=(1-sum(test$Target_without_mask==pred.lnnetO)/nrow(test))
#matrice de confusion
TC.lnnetO=table(test$Target_without_mask,pred.lnnetO)