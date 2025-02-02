# SVM polynomial optimal
library(pls)
library(e1071)

#creation des données d'apprentissage
data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:15)])
names(data.acp)[1] = "Target_without_mask"

#Echantillonage
sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
train= data.acp[sample, ] #75% de lignes
test= data.acp[-sample, ] #25% de ligne

#Application de svm
res.svm.PolyO=svm(Target_without_mask~.,data=train, degree=20 ,kernel="polynomial", epsilon=.3)

#prédiction
pred.svm.PolyO=predict(res.svm.PolyO,test,type="class")

#Taux de mal classé
Tmc.svm.PolyO=1-sum(test$Target_without_mask==pred.svm.PolyO)/nrow(test)

#matrice de confusion
TCP.svm.PolyO=table(test$Target_without_mask,pred.svm.PolyO)
