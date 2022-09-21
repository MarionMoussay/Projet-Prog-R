require(nnet)
require(tidyverse)
require(cluster)
require(factoextra)
require(ggplot2)
require(FactoMineR)
require(corrplot)

stars <- read.table("data/stars.csv", sep=",", header=TRUE)
str(stars)

# Nettoyage de Marie: ----------------------------
stars$Star.type <- as.factor(stars$Star.type)
stars$Star.color <- as.factor(stars$Star.color)
stars$Spectral.Class <- as.factor(stars$Spectral.Class)

levels(stars$Star.type)
levels(stars$Star.color)
levels(stars$Spectral.Class)

stars$Star.color <- as.character(stars$Star.color)

stars$Star.color[which(stars$Star.color=="Blue ")] <- "Blue"
stars$Star.color[c(which(stars$Star.color=="Blue white"),(which(stars$Star.color=="Blue white ")),(which(stars$Star.color=="Blue White")),(which(stars$Star.color=="Blue-White")))] <- "Blue-white"
stars$Star.color[which(stars$Star.color=="white")] <- "White"
stars$Star.color[which(stars$Star.color=="yellowish")] <- "Yellowish"

stars$Star.color <- as.factor(stars$Star.color)
levels(stars$Star.color)

# -------------------------------------------------

# On traite utilise pas couleur qui est elle même expliquée par la température:
levels(stars$Star.color) <- 1:19
stars$Star.color <- as.numeric(stars$Star.color)
cor(stars$Temperature..K., stars$Star.color) #forte corrélation négative 


## Reg log
ind <- sample(1:80)
data.train <- stars[ind, ]
data.test <- stars[-ind,]
mod <- nnet::multinom(Star.type~Temperature..K.+ Luminosity.L.Lo. + Radius.R.Ro. + Absolute.magnitude.Mv., data=data.train)

pred.stars <- predict(mod, newdata = data.test)
mean(pred.stars != data.test$Star.type) #Accuracy de 85%
# AIC(mod)
# BIC(mod) -> pas ouf 

## ACP 
data.acp <- stars[,-5]
res.pca <- PCA(data.acp, quali.sup = c(5,6)) 
plotellipses(res.pca) # Je sais pas trop interpréter 

## Classification non supervisée 
matrice <- stars %>% select(Temperature..K.,Luminosity.L.Lo.,Radius.R.Ro.,Absolute.magnitude.Mv.)

d = dist(matrice)
cah.ward = hclust(d, method = "ward.D")
cah.min <- hclust(d, method = "single")
cah.max <- hclust(d, method = "complete")

plot(cah.ward, hang = -1, main = "Distance de Ward", ylab = " ",xlab=" ", labels = FALSE)
plot(cah.min, hang = -1, main = "Distance du saut minimal", ylab = " ",xlab=" ",labels = FALSE)
plot(cah.max, hang = -1, main = "Distance du saut maximal", ylab = " ",xlab=" ",labels = FALSE)

plot(rev(cah.ward$height)[1:20],type="b",xlab ='Number of cluster', ylab='Inertie inter',main="Fonction de perte avec le critère de Ward" )

# On prend K=3
km <- kmeans(matrice, centers = 3)
fviz_nbclust(matrice, kmeans, "gap_stat", k.max = 10) + labs(subtitle = "Gap Statistic method") #4 groupe
fviz_nbclust(matrice, kmeans, "silhouette", k.max = 15) + labs(subtitle = "Silhouette method") #3 groupes 
fviz_nbclust(matrice, kmeans, method = "wss", k.max = 15) + labs(subtitle = "Elbow method") #4 groupes

# On représente les 4 groupes 
gpe.ward <- cutree(cah.ward, k = 4)
clusplot(matrice, gpe.ward, labels = 4, col.p = as.numeric(gpe.ward))

asteroides <- read.table("data/nasa.csv", sep=",", header=TRUE)
data.asteroides <- asteroides %>% select(-Close.Approach.Date, -Orbiting.Body, -Orbit.Determination.Date,-Equinox )
PCA(data.asteroides, quali.sup=36)
