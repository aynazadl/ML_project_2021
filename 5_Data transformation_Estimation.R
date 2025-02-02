################################################################################################
# Comparaison réalité terrain / image estimée
################################################################################################
# reconstruction de l'image estimée pour une pdrédiction de l'output dénomée Estimations
nbrows=610
nbcols=340
Image_estimation = rep(0, nbpixels)
Image_estimation[pixels_id_without_mask] = Estimations
IMAGE_ESTIMATION = matrix(Image_estimation, nrow = nbrows, ncol = nbcols)
# représentation des 2 images (réalité terrain + image estimée par l'arbre de décision construit)
par(mar = c(2,0.5,2,0), cex.main = 2)
layout(matrix(c(1,1,1,2,3,3,3,4), nrow = 2, ncol = 4, byrow = TRUE))
Colors = c("pink", "green3", grey(0.4), "green4", "darkorchid2", "brown", "darkorchid4", "white", grey(0.7))
# image terrain
image(pavia_gt$paviaU.gt, useRaster=TRUE, axes=FALSE, col = c("black", Colors), main = "Réalité terrain")
# simule un graphique pour pouvoir représenter une légende
plot(1:1, 2:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend.names = c("mask", "asphalt", "meadows", "gravel", "trees", "metal sheets", "bare soil", "bitumen", "pavement", "shadows")
legend("bottomleft", fill = c("black", Colors), legend = legend.names, pch = 3, bty = "n", cex = 2)
# image estimée
image(IMAGE_ESTIMATION, useRaster=TRUE, axes=FALSE, col = c("black", Colors), main = "Image estimée")
#
