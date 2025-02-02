# KNN optimal
library(class)

#creation des données d'apprentissage
data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:15)])
names(data.acp)[1] = "Target_without_mask"

sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
train= data.acp[sample, ] #75% de lignes
test= data.acp[-sample, ] #25% de ligne

#Application de knn optimal
res.knnO=knn(train,test,train$Target_without_mask,k=k,prob=FALSE)
#Table confusion
confusionknnO=table(test$Target_without_mask,res.knnO)
#Taux de mal classé
Tmc=1-sum(diag(confusionknnO))/sum(confusionknnO)