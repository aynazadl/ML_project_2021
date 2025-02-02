#SVM Radial
library(pls)
library(e1071)

########################################
gam= c(0.1,0.2,0.4,0.6) #gamma


k1=0 #compteur des lignes

mse.svmRadial=matrix(nrow = length(gam), ncol = 2)


for(g in gam){#pour chaque ajout de dimension
  
  sample= sample.int(n = nrow(DATA), size = floor(.75*nrow(DATA)), replace = F)
  train= DATA[sample, ] #75% de lignes
  test= DATA[-sample, ] #25% de ligne
  
  #Application de svm
  res.svmRadial=svm(Target_without_mask~.,data=train, gamma=g,kernel="radial", epsilon=.3)
  
  #prédiction
  predsvm=predict(res.svmRadial,test,type="class")
  #Taux de mal classé
  TmcSVMradiaAvant=1-sum(test$Target_without_mask==predsvm)/nrow(test)
 # construction de la matrice
  k1=k1+1
  mse.svmRadial[k1,1]=g
  mse.svmRadial[k1,2]=TmcSVMradiaAvant*100
    }


error.svmRadial=data.frame(mse.svmRadial)
names(error.svmRadial)[1] = "Gamma"
names(error.svmRadial)[2] = "TmcSVMradiaAvant"
error.svmRadial %>% group_by(dim) %>% summarise(TmcSVMradiaAvant = mean(TmcSVMradiaAvant))
subset(error.svmRadial, TmcSVMradiaAvant == min(error.svmRadial$TmcSVMradiaAvant))
#####################################################################################################



