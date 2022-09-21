stars <- read.table("data/stars.csv", sep=",", header=TRUE)
# str(stars)

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

write.csv(stars, "data/clean_stars.csv")

# -------------------------------------------------