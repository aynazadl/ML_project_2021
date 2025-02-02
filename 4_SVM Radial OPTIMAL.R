# SVM Radial optimal
library(pls)
library(e1071)

#creation des données d'apprentissage
data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:20)])
names(data.acp)[1] = "Target_without_mask"

#Echantillonage
sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
train= data.acp[sample, ] #75% de lignes
test= data.acp[-sample, ] #25% de ligne

#Application de svm
res.svm.PolyR=svm(Target_without_mask~.,data=train, gamma=0.1 ,kernel="radial", epsilon=.3)

#prédiction
pred.svm.PolyR=predict(res.svm.PolyR,test,type="class")

#Taux de mal classé
Tmc.svm.PolyR=1-sum(test$Target_without_mask==pred.svm.PolyR)/nrow(test)

#matrice de confusion
TCP.svm.PolyR=table(test$Target_without_mask,pred.svm.PolyR)
