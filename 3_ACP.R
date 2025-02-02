#ACP
library(FactoMineR)
library(factoextra)

Paviacp=PCA(DATA,quali.sup=1,ncp = 25, graph = FALSE)#ncp le nombre des dimension gardé en résultat

#Inertie
plot(Paviacp$eig[,3])
abline(a=99, b=0)
#Affichage des classes d'individus

plot.PCA(Paviacp, axes=c(1, 2), choix="ind", habillage=1)
fviz_pca_ind(Paviacp,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$Target_without_mask, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Nature Pixel"
)
#Affichage d'un biplot
fviz_pca_biplot(Paviacp, 
                col.ind = DATA$Target_without_mask, palette = "jco", 
                addEllipses = TRUE, label = "var",
                legend.title = "Target") 

#choix d'axe pour l'analyse
fviz_eig(Paviacp, addlabels = TRUE, ylim = c(0, 50))

corrplot(Paviacp$var$cos2, is.corr=FALSE)

fviz_cos2(Paviacp, choice = "var", axes = 1:2)
fviz_contrib(Paviacp, choice = "var", axes = 1:2)