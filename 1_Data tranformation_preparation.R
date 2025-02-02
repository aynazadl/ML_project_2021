# PROJET IMAGE HYPERSPECTRALE PAVIA 
################################################################################
# 0 --> mask (i.e. pixels non concernés par l'étude)
# 1 --> asphalt (6631)
# 2 --> meadows (18649)
# 3 --> Gravel (2009)
# 4 --> Trees (3064)
# 5 --> painted metal sheets (1345)
# 6 --> bare soil (5029)
# 7 --> bitumen (1330)
# 8 --> self-blocking bricks (3682)
# 9 --> shadows (947)
################################################################################
################################################################################
# TELECHARGER ET FORMATER LES DONNEES
################################################################################

## Chargement des données
#########################
# utilisation du package "R.matlab" pour lire une image au format du logiciel Matlab
# Télécharger le package "R.matlab" puis charger le dans votre session à l'aide de la commande
library(R.matlab)

# Récupération des données
#  Définition du chemin d'accès aux données
#path = "c:\Users\zaim_h\Desktop\M2\BI-ERP\DataMining\Projet DataMining"
setwd("C:/Users/zaim_h/Desktop/M2/BI-ERP/DataMining/Projet DataMining")

# lecture du fichier "PaviaU_gt.mat" donnant la nature des pixels (image 610 x 340 pixels)
##Output
pavia_gt = readMat("PaviaU_gt.mat")

# lecture de l'image hyperspectrale
##Input
pavia_hyp = readMat("PaviaU.mat")
##################################
## Formatage de l'image hyperspectrale
######################################
# les commandes suivantes transforment le tableau tri-dimensionnel "pavia_hyp$paviaU" 
# en une matrice possédant 207400 lignes x 103 colonnes (207400 = 610 x 340)
count = 0
nbpixels = 610 * 340
PAVIA_HYP = matrix(0, nrow = nbpixels, ncol = 103)
for(jj in 1:340)
  for(ii in 1:610){
    count = count + 1
    PAVIA_HYP[count, ] = pavia_hyp$paviaU[ii, jj, ]
  }
# désignation des prédicteurs
nbwavelengths = ncol(PAVIA_HYP) # nombre de prédicteurs
dimnames(PAVIA_HYP)[[2]] = paste0("V", 1:nbwavelengths)
# "pixels_id" identifie chaque pixel par un numéro correspondant à sa localisation dans l'image
pixels_id = 1:nbpixels
# "Target" contient l'output
Target = as.vector(pavia_gt$paviaU.gt)
#Construction de la matrice de données par élimination du masque
# matrice contenant les prédicteurs uniquement pour les pixels en dehors du masque (ceux pour lesquels la modalité de "Target" est "0")
PAVIA_HYP_WITHOUT_MASK = PAVIA_HYP[Target != 0, ]
# variable cible uniquement pour les pixels en dehors du masque
Target_without_mask = Target[Target != 0]
# identifiant uniquement pour les pixels en dehors du masque
pixels_id_without_mask = pixels_id[Target != 0]
# Définition de "Target_without_mask" comme facteur
Target_without_mask = as.factor(Target_without_mask)
# Jeu de données final contenant à la fois la variable cible et les prédicteurs. 
# L'objet créé est mis au format "dataframe"
DATA = data.frame(Target_without_mask, PAVIA_HYP_WITHOUT_MASK)
summary(DATA)
colnames(DATA)