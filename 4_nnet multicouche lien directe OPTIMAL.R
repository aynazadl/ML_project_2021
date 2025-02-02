#Réseau de neurone multicouche avec lien directe OPTIMAL
library(rpart)
library(rpart.plot)
library(nnet)

#creation des données d'apprentissage
data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
names(data.acp)[1] = "Target_without_mask"

#Echantillonage
sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
train= data.acp[sample, ] #75% de lignes
test= data.acp[-sample, ] #25% de lignes

#Application de nnet AVEC lien linéaire nnet COUCHE WITH LINK
res.nnet.cwlO = nnet(Target_without_mask~., data=train ,size=s, 
                   skip=FALSE,rang=0.1,linout=TRUE,
                   decay=5e-4,maxit=2000,MaxNWts=3500)
#prédiction
pred.nnet.cwlO=predict(res.nnet.cwlO, test, type="class")
#Taux de mal classé
Tmc=(1-sum(test$Target_without_mask==pred.nnet.cwlO)/nrow(test))