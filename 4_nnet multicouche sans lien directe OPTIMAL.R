#Réseau de neurone multicouche sans lien directe
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

#Application de nnet sans lien linéaire "nnet couche without link"
res.nnetcwtl = nnet(Target_without_mask~., data=train ,size=nc[s], 
                    skip=FALSE,rang=0.1,linout=TRUE,
                    decay=5e-4,maxit=1000,MaxNWts=1500)

#prédiction
nnetcwtlpred=predict(res.nnetcwtl, test, type="class")

#Taux de mal classé
Tmc=(1-sum(test$Target_without_mask==nnetcwtlpred)/nrow(test))
