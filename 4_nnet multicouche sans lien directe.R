#Réseau de neurone multicouche sans lien directe
library(rpart)
library(rpart.plot)
library(nnet)

nd=c(3,5,10,15,20) #nb dimension
nr=2 #nb replication
nc=c(1,5,15,17) #vecteur des noeud à tester
k1=0 #compteur des lignes

mse.nnetcwtl=matrix(nrow = length(nd)*nr*length(nc), ncol = 3)


for(i in 1:length(nd)){#pour chaque ajout de dimension
  for (s in 1:length(nc)){ #pour chaque choix de noeud
    
    #creation des données d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de lignes
      
      #Application de nnet sans lien linéaire "nnet couche without link"
      res.nnetcwtl = nnet(Target_without_mask~., data=train ,size=s, 
                      skip=FALSE,rang=0.1,linout=TRUE,
                      decay=5e-4,maxit=2500,MaxNWts=3000)
      #prédiction
      nnetcwtlpred=predict(res.nnetcwtl, test, type="class")
      #Taux de mal classé
      Tmc=(1-sum(test$Target_without_mask==nnetcwtlpred)/nrow(test))
      #construction de la matrice
      k1=k1+1
      mse.nnetcwtl[k1,1]=i #DIM
      mse.nnetcwtl[k1,2]=Tmc
      mse.nnetcwtl[k1,3]=s #choix de noeud
    }
  }
}

error.nnetcwtl=data.frame(mse.nnetcwtl)
names(error.nnetcwtl)[1] = "dim"
names(error.nnetcwtl)[2] = "Tmc"
names(error.nnetcwtl)[3] = "size"

error.nnetcwtl %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.nnetcwtl %>% group_by(size) %>% summarise(Tmc = mean(Tmc))

subset(error.nnetcwtl, Tmc == min(error.nnetcwtl$Tmc)) #chercher le ligne qui donne le min de TMC