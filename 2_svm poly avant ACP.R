########## polynomial Avant ACP

library(pls)
library(e1071)


######################################## plynomial
dg= c(1,2,3) #degree


k1=0 #compteur des lignes

mse.svmPolynomial=matrix(nrow = length(dg), ncol = 2)


for(d in dg){# #pour chaque gamma
  
  sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
  train= DATA[sample, ] #75% de lignes
  test= DATA[-sample, ] #25% de ligne
  
  
  
  #Application de svm
  res.svmPolynomial=svm(Target_without_mask~.,data=train, degree=dg,kernel="polynomial", epsilon=.3)
  
  #prédiction
  predsvm=predict(res.svmPolynomial,test,type="class")
  
  #Tasux de mal classé
  Tmc=1-sum(test$Target_without_mask==predsvm)/nrow(test)
  
  #construction de la matrice
  k1=k1+1
  mse.svmPolynomial[k1,1]=d
  mse.svmPolynomial[k1,2]=Tmc*100
}

error.svmPolynomial=data.frame(mse.svmPolynomial)
names(error.svmPolynomial)[1] = "degree"
names(error.svmPolynomial)[2] = "Tmc"
error.svmPolynomial %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
subset(error.svmPolynomial, Tmc == min(error.svmPolynomial$Tmc))
