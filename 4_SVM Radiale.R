#SVM Radial
library(pls)
library(e1071)

########################################
gam= c(0.1,0.2,0.4,0.6) #gamma
nd = c(10,15,20) # nb de dimension
nr=2 #nb replication

k1=0 #compteur des lignes

mse.svmRadial=matrix(nrow = length(gam)*nr*length(nd), ncol = 3)


for(i in nd){#pour chaque ajout de dimension
  for (g in gam){ #pour chaque gamma
    
    #creation des donnÃ©es d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque rÃ©plication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de ligne
      
      #Application de svm
      res.svmRadial=svm(Target_without_mask~.,data=train, gamma=g,kernel="radial", epsilon=.3)
      
      #prédiction
      predsvm=predict(res.svmRadial,test,type="class")
      #Taux de mal classé
      Tmc=1-sum(test$Target_without_mask==predsvm)/nrow(test)
      
      #construction de la matrice
      k1=k1+1
      mse.svmRadial[k1,1]=i
      mse.svmRadial[k1,2]=g
      mse.svmRadial[k1,3]=Tmc*100
    }
  }
}


error.svmRadial=data.frame(mse.svmRadial)
names(error.svmRadial)[1] = "dim"
names(error.svmRadial)[2] = "Gamma"
names(error.svmRadial)[3] = "Tmc"
error.svmRadial %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
subset(error.svmRadial, Tmc == min(error.svmRadial$Tmc))
#####################################################################################################