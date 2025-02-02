# KNN optimal
library(class)

sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
train= DATA[sample, ] #75% de lignes
test= DATA[-sample, ] #25% de ligne

#Application de knn optimal
res.knnO1=knn(train,test,train$Target_without_mask,k=5,prob=FALSE)
#Table confusion
confusionknnO1=table(test$Target_without_mask,res.knnO1)
#Taux de mal classé
Tmc=1-sum(diag(confusionknnO1))/sum(confusionknnO1)