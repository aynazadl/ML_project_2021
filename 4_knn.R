# KNN
library(class)

nk= c(1,5,10,15) #nombre de voisin
nd = c(10,15,20) # nb de dimension
nr=2 #nb replication

k1=0 #compteur des lignes

mse.knn=matrix(nrow = length(nk)*nr*length(nd), ncol = 3)


for(i in nd){#pour chaque ajout de dimension
  for (k in nk){ #pour chaque gamma
    
    #creation des données d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de ligne
      
      #Application de knn 
      res.knn=knn(train,test,train$Target_without_mask,k=k,prob=FALSE)
      #Table confusion
      confusionknn=table(test$Target_without_mask,res.knn)
      #Taux de mal classé
      Tmc=1-sum(diag(confusionknn))/sum(confusionknn)
      
      #construction de la matrice
      k1=k1+1
      mse.knn[k1,1]=i
      mse.knn[k1,2]=k
      mse.knn[k1,3]=Tmc*100
    }
  }
}

error.knn=data.frame(mse.knn)
names(error.knn)[1] = "dim"
names(error.knn)[2] = "nbk"
names(error.knn)[3] = "Tmc"
error.knn %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
subset(error.knn, Tmc == min(error.knn$Tmc))