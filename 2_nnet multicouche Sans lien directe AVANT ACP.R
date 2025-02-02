#Réseau de neurone
library(rpart)
library(rpart.plot)
library(nnet)

#nd=c(3,5,10,15) #nb dimension
#nr=2 #nb replication
nc=c(1,5,7,8,10,11) #vecteur des noeud à tester
k1=0 #compteur des lignes

mse.nnetbacplien=matrix(nrow = length(nc), ncol = 2)


#for(i in 1:length(nd)){#pour chaque ajout de dimension
for (s in nc){ #pour chaque choix de noeud
  
  #creation des données d'apprentissage
  #DATA = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
  #  names(data.acp)[1] = "Target_without_mask"
  
  #for(j in 1:nr){ #pour chaque réplication
  #Echantillonage
  sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
  train= DATA[sample, ] #75% de lignes
  test= DATA[-sample, ] #25% de lignes
  
  #Application de nnet sans lien linéaire
  res.nnetbacplien= nnet(Target_without_mask~., data=train ,size=s,
                      skip=FALSE,rang=0.1,linout=TRUE,
                      decay=5e-4,maxit=1000,MaxNWts=1500)
  #prédiction
  nnetpredbacplien=predict(res.nnetbacplien, test, type="class")
  #Taux de mal classé
  Tmc=(1-sum(test$Target_without_mask==nnetpredbacplien)/nrow(test))
  #construction de la matrice
  k1=k1+1
  #mse.nnetbacplien[k1,1]=i #DIM
  mse.nnetbacplien[k1,1]=Tmc
  mse.nnetbacplien[k1,2]=s #choix de noeud
}
#}
#}

error.nnetbacplien=data.frame(mse.nnetbacplien)
#names(error.nnetbacplien)[1] = "dim"
names(error.nnetbacplien)[1] = "Tmc"
names(error.nnetbacplien)[2] = "size"

error.nnetbacplien %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.nnetbacplien %>% group_by(size) %>% summarise(Tmc = mean(Tmc))

subset(error.nnetbacplien, Tmc == min(error.nnetbacplien$Tmc)) #chercher le ligne qui donne le min de TMC
