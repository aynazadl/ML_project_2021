#R�seau de neurone lin�aire Avant
library(rpart)
library(rpart.plot)
library(nnet)


#nc=c(1,5,15,17) #vecteur des noeud � tester
k1=0 #compteur des lignes

#creation des donn�es d'apprentissage
    
#Echantillonage
sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
train= DATA[sample, ] #75% de lignes
test= DATA[-sample, ] #25% de lignes
      
#Application de nnet lin�aire 
res.lnnetAvACPO = nnet(Target_without_mask~., data=train ,size=0, 
skip=TRUE,rang=0.1,linout=TRUE,
decay=5e-4,maxit=1000,MaxNWts=1500)
#pr�diction
lnnetpredO=predict(res.lnnetAvACPO, test, type="class")
 #Taux de mal class�
Tmc=(1-sum(test$Target_without_mask==lnnetpredO)/nrow(test)
      