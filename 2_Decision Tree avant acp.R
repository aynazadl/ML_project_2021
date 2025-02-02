##Arbre de décision avant acp
library(rpart)
library(rpart.plot) 



nsplit=c(5,10,15,20) #nombre des split à tester
k1=0 #compteur des lignes

mse.dt=matrix(nrow = length(nsplit), ncol = 2)

for(ns in nsplit){#pour chaque ajout de dimension
  
  sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
  train= DATA[sample, ] #75% de lignes
  test= DATA[-sample, ] #25% de lignes
  
  #Application d'arbre de décision
  res.dt=rpart(Target_without_mask~.,data=train,control=rpart.control(minsplit=ns,cp=.05))
  
  #prédiction
  dtpred=predict(res.dt, test, type="class")
  
  #Taux de mal classé
  Tmc=(1-sum(test$Target_without_mask==dtpred)/nrow(test))
  
 
    #construction de la matrice
    k1=k1+1
    mse.dt[k1,1]=ns #Chois de nsplit
    mse.dt[k1,2]=Tmc

    }
  
    
error.dt=data.frame(mse.dt)
names(error.dt)[1] = "split"
names(error.dt)[2] = "Tmc"
   
    
error.dt %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.dt %>% group_by(size) %>% summarise(Tmc = mean(Tmc))
    
subset(error.dt, Tmc == min(error.dt$Tmc)) #chercher le ligne qui donne le min de TMC
