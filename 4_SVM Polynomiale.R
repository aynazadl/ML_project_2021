# SVM polynomial
library(pls)
library(e1071)

dg= c(1,2,3) #degree
nd = c(10,15,20) # nb de dimension
nr=2 #nb replication

k1=0 #compteur des lignes

mse.svm.Poly=matrix(nrow = length(dg)*nr*length(nd), ncol = 3)


for(i in nd){#pour chaque ajout de dimension
  for (d in dg){ #pour chaque gamma
    
    #creation des données d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de ligne
      
      #Application de svm
      res.svm.Poly=svm(Target_without_mask~.,data=train, degree=dg,kernel="polynomial", epsilon=.3)
      
      #prédiction
      pred.svm.Poly=predict(res.svm.Poly,test,type="class")
      #Tasux de mal classé
      Tmc=1-sum(test$Target_without_mask==pred.svm.Poly)/nrow(test)
      
      #construction de la matrice
      k1=k1+1
      mse.svm.Poly[k1,1]=i
      mse.svm.Poly[k1,2]=d
      mse.svm.Poly[k1,3]=Tmc*100
    }
  }
}
error.svm.Poly=data.frame(mse.svm.Poly)
names(error.svm.Poly)[1] = "dim"
names(error.svm.Poly)[2] = "degree"
names(error.svm.Poly)[3] = "Tmc"
error.svm.Poly %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
subset(error.svm.Poly, Tmc == min(error.svm.Poly$Tmc))