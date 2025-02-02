#Arbre de décision
library(rpart)
library(rpart.plot)


#Echantillonage
sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
train= DATA[sample, ] #75% de lignes
test= DATA[-sample, ] #25% de lignes

#Application d'arbre de décision
res.dt=rpart(Target_without_mask~.,data=train,control=rpart.control(minsplit=20,cp=.05))

#prédiction
pred.dtO=predict(res.dt, test, type="class")

#Taux de mal classé
Tmc.dtO=(1-sum(test$Target_without_mask==pred.dtO)/nrow(test))

#matrice de confusion
TC.dtO=table(test$Target_without_mask,pred.dtO)
