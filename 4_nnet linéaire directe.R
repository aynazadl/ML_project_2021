#Réseau de neurone linéaire
library(rpart)
library(rpart.plot)
library(nnet)

nd=c(3,5,10,15,18,20,22,24,25) #nb dimension
nr=2 #nb replication
#nc=c(1,5,15,17) #vecteur des noeud à tester
k1=0 #compteur des lignes

mse.lnnet=matrix(nrow = length(nd)*nr, ncol = 2)


for(i in nd){#pour chaque ajout de dimension
  #for (s in 1:length(nc)){ #pour chaque choix de noeud
    
    #creation des données d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de lignes
      
      #Application de nnet linéaire 
      res.lnnet = nnet(Target_without_mask~., data=train ,size=0, 
                         skip=TRUE,rang=0.1,linout=TRUE,
                         decay=5e-4,maxit=1000,MaxNWts=1500)
      #prédiction
      lnnetpred=predict(res.lnnet, test, type="class")
      #Taux de mal classé
      Tmc=(1-sum(test$Target_without_mask==lnnetpred)/nrow(test))
      #construction de la matrice
      k1=k1+1
      mse.lnnet[k1,1]=i #DIM
      mse.lnnet[k1,2]=Tmc
      #mse.lnnet[k1,3]=s #choix de noeud
    }
  }
#}

error.lnnet=data.frame(mse.lnnet)
names(error.lnnet)[1] = "dim"
names(error.lnnet)[2] = "Tmc"
#names(error.lnnet)[3] = "size"

error.lnnet %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.lnnet %>% group_by(size) %>% summarise(Tmc = mean(Tmc))

subset(error.lnnet, Tmc == min(error.lnnet$Tmc)) #chercher le ligne qui donne le min de TMC