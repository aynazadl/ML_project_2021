#Arbre de décision
library(rpart)
library(rpart.plot)

nd=c(3,5,10,15,16,20) #nb dimension
nr=2 #nb replication
nsplit=c(5,10,15,20) #nombre des split à tester
k1=0 #compteur des lignes

mse.dt=matrix(nrow = length(nd)*nr*length(nsplit), ncol = 3)


for(i in nd){#pour chaque ajout de dimension
  for (ns in nsplit){ #pour chaque choix de split
    
    #creation des données d'apprentissage
    data.acp = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
    names(data.acp)[1] = "Target_without_mask"
    
    for(j in 1:nr){ #pour chaque réplication
      #Echantillonage
      sample= sample.int(n = nrow(data.acp), size = floor(.75*nrow(data.acp)), replace = F)
      train= data.acp[sample, ] #75% de lignes
      test= data.acp[-sample, ] #25% de lignes
      
      #Application d'arbre de décision
      res.dt=rpart(Target_without_mask~.,data=train,control=rpart.control(minsplit=ns,cp=.05))
      
      #prédiction
      pred.dt=predict(res.dt, test, type="class")
      
      #Taux de mal classé
      Tmc=(1-sum(test$Target_without_mask==pred.dt)/nrow(test))
      #construction de la matrice
      k1=k1+1
      mse.dt[k1,1]=i #DIM
      mse.dt[k1,2]=Tmc
      mse.dt[k1,3]=ns #choix de split
    }
  }
}

error.dt=data.frame(mse.dt)
names(error.dt)[1] = "dim"
names(error.dt)[2] = "Tmc"
names(error.dt)[3] = "split"

error.dt %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.dt %>% group_by(size) %>% summarise(Tmc = mean(Tmc))

subset(error.dt, Tmc == min(error.dt$Tmc)) #chercher le ligne qui donne le min de TMC