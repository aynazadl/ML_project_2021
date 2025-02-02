#??? KNN
library(class)

nk= c(1,5,10,15) #nombre de voisin
#nd = c(10,15,20) # nb de dimension
nr=2 #nb replication

k1=0 #compteur des lignes

mse.knnbacp=matrix(nrow = length(nk)*nr, ncol = 2)


#for(i in nd){#pour chaque ajout de dimension
  for (k in nk){ #pour chaque gamma
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
      train= DATA[sample, ] #75% de lignes
      test= DATA[-sample, ] #25% de ligne
      
      #Application de knn 
      
      res.knnbacp=knn(train,test,train$Target_without_mask,k=k,prob=FALSE)
      
      confusionknnbacp=table(test$Target_without_mask,res.knnbacp)
      Tmc=1-sum(diag(confusionknnbacp))/sum(confusionknnbacp)
      
      #construction de la matrice
      k1=k1+1
      #mse.knn[k1,1]=i
      mse.knnbacp[k1,1]=k
      mse.knnbacp[k1,2]=Tmc*100
    }
  }
#}

error.knnbacp=data.frame(mse.knnbacp)
#names(error.knn)[1] = "dim"
names(error.knnbacp)[1] = "nbk"
names(error.knnbacp)[2] = "Tmc"
error.knnbacp %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
subset(error.knnbacp, Tmc == min(error.knnbacp$Tmc))