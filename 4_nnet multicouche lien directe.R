#Réseau de neurone multicouche avec lien directe
library(rpart)
library(rpart.plot)
library(nnet)

nd=c(3,5,10,15,20) #nb dimension
nr=2 #nb replication
nc=c(1,5,15,17) #vecteur des noeud à tester
k1=0 #compteur des lignes

mse.nnetcwl=matrix(nrow = length(nd)*nr*length(nc), ncol = 3)


for(i in nd){#pour chaque ajout de dimension
  for (s in nc){ #pour chaque choix de noeud
    
    #creation des données d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de lignes
      
      #Application de nnet AVEC lien linéaire nnet COUCHE WITH LINK
      res.nnetcwl = nnet(Target_without_mask~., data=train ,size=s, 
                      skip=TRUE,rang=0.1,linout=TRUE,
                      decay=5e-4,maxit=2000,MaxNWts=3500)
      #prédiction
      nnetcwlpred=predict(res.nnetcwl, test, type="class")
      #Taux de mal classé
      Tmc=(1-sum(test$Target_without_mask==nnetcwlpred)/nrow(test))
      #construction de la matrice
      k1=k1+1
      mse.nnetcwl[k1,1]=i #DIM
      mse.nnetcwl[k1,2]=Tmc #Taux de mal classé
      mse.nnetcwl[k1,3]=s #choix de noeud
    }
  }
}

error.nnetcwl=data.frame(mse.nnetcwl)
names(error.nnetcwl)[1] = "dim"
names(error.nnetcwl)[2] = "Tmc"
names(error.nnetcwl)[3] = "size"

error.nnetcwl %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.nnetcwl %>% group_by(size) %>% summarise(Tmc = mean(Tmc))
subset(error.nnetcwl, Tmc == min(error.nnetcwl$Tmc)) #chercher le ligne qui donne le min de TMC