#Arbre de décision
library(rpart)
library(rpart.plot)
    
#creation des données d'apprentissage
data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:5)])
names(data.acp)[1] = "Target_without_mask"

#Echantillonage
sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
train= data.acp[sample, ] #75% de lignes
test= data.acp[-sample, ] #25% de lignes

#Application d'arbre de décision
res.dt=rpart(Target_without_mask~.,data=train,control=rpart.control(minsplit=10,cp=.05))

#prédiction
pred.dtO=predict(res.dt, test, type="class")

#Taux de mal classé
Tmc.dtO=(1-sum(test$Target_without_mask==pred.dtO)/nrow(test))

#matrice de confusion
TC.dtO=table(test$Target_without_mask,pred.dtO)
