#R�seau de neurone
library(rpart)
library(rpart.plot)
library(nnet)

#nd=c(3,5,10,15) #nb dimension
#nr=2 #nb replication
nc=c(1,2,3,4) #vecteur des noeud � tester
k1=0 #compteur des lignes

mse.nnetbacps=matrix(nrow = length(nc), ncol = 2)


#for(i in 1:length(nd)){#pour chaque ajout de dimension
  for (s in nc){ #pour chaque choix de noeud
    
    #creation des donn�es d'apprentissage
   #DATA = data.frame(Target_without_mask, Paviacp$ind$coord[,c(1:i)])
  #  names(data.acp)[1] = "Target_without_mask"
    
    #for(j in 1:nr){ #pour chaque r�plication
      #Echantillonage
      sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
      train= DATA[sample, ] #75% de lignes
      test= DATA[-sample, ] #25% de lignes
      
      #Application de nnet sans lien lin�aire
      res.nnetbacps= nnet(Target_without_mask~., data=train ,size=s, 
                      skip=TRUE,rang=0.1,linout=TRUE,
                      decay=5e-4,maxit=2000,MaxNWts=3500)
      #pr�diction
      nnetpredbacps=predict(res.nnetbacps, test, type="class")
      #Taux de mal class�
      Tmc=(1-sum(test$Target_without_mask==nnetpredbacps)/nrow(test))
      #construction de la matrice
      k1=k1+1
      #mse.nnetbacps[k1,1]=i #DIM
      mse.nnetbacps[k1,1]=Tmc
      mse.nnetbacps[k1,2]=s #choix de noeud
    }
  #}
#}

error.nnetbacps=data.frame(mse.nnetbacps)
#names(error.nnetbacps)[1] = "dim"
names(error.nnetbacps)[1] = "Tmc"
names(error.nnetbacps)[2] = "size"

error.nnetbacps %>% group_by(dim) %>% summarise(Tmc = mean(Tmc))
error.nnetbacps %>% group_by(size) %>% summarise(Tmc = mean(Tmc))

subset(error.nnetbacps, Tmc == min(error.nnetbacps$Tmc)) #chercher le ligne qui donne le min de TMC

