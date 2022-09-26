#Chargement du jeu de données nétoyé par Marie
stars_by_Marie <- read.csv("./data/stars_by_Marie.csv")
stars_by_Marie <- stars
dim(stars_by_Marie)
data <- stars_by_Marie[,1:5]

#Je voulais juste regarder la fréquence des différentes étoiles
#il faut copier coller le html
library(FREQ)
library(frequency)
require(nnet)
require(leaps)

data$Star_Type = factor(data$Star_Type)
freq(data$Star_Type)
str(data$Star_Type)
levels(data$Star_Type)

#Modele de regression multinom

mod_tot <- multinom(Star_Type~.,data=data)
mod_tot
mod_null <- multinom(Star_Type ~ 1, data=data)
mod_null

##odds.ratio(mod_tot) je sais pas le package 

require(RcmdrMisc) 
select <- stepwise(mod_tot,data=data,direction="forward/backward", criterion="AIC", trace=0)
select
Anova(select)

length(coef(select))

require(pls)             # For segments
require(groupdata2)      # For fold
require(boot)            # For cv.glm

n = nrow(data)                   # Sample size
segments = pls::cvsegments(k=10,N=n) # Defines a list of 10 random segments
segments

cvpredictions = rep(0,n)   # Initialize a n-vector of predicted Star_type 
for (k in 1:10) {
  train = data[-segments[[k]],]   # Training dataset
  test = data[segments[[k]],]     # Test dataset
  mod <- multinom(Star_Type~.,data=train)
  select <- stepwise(mod,data=data,direction="forward/backward", criterion="AIC", trace=0)
  # "nombre de variables sélectionnés:15" ??
  print(paste0("nombre de variables sélectionnés:", length(coef(select)-1)))
  bestmod = multinom(formula(select), data=train)   # Fits the best submodel
  cvpredictions[segments[[k]]] = predict(bestmod,newdata=test)
}

select = summary(regsubsets(Star_Type~.,data=data,nvmax=9))
select$which

### Best sub-model with one variable
colnames(select$which)[select$which[1,]]

colnames(select$which)[select$which[2,]]

colnames(select$which)[select$which[3,]]

### RSS plot for exhaustive feature selection
plot(1:4,select$rss,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab="Residual sum-of-squares",main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
grid()

### Equivalent R2 plot
plot(1:4,select$rsq,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab=expression(R^2),main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
grid()

bic = select$bic                            # BIC
aic = bic - (log(nrow(data))-2)*(1:4)       # AIC

# BIC-AIC plot for exhaustive feature selection
plot(1:4,bic,pch=16,bty="l",type="b",xlab="Number of explanatory variables",
     ylab="Information criterion",ylim=range(c(aic,bic)),col="darkgray",
     main="Exhaustive model selection",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
lines(1:4,aic,type="b",pch=17,lwd=2,col="coral1")
legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("darkgray","coral1"),bty="n",cex=1.25,legend=c("BIC","AIC"))
grid()

selected = select$which[which.min(bic),]   # Indices of selected variables
selected

# pb avec ces 2 dernières lignes :/ -> je les ai écris à la main du coup
bestmod = multinom(data$Star_Type~.,data=data[,c(1,3,4)])
coef(bestmod)

bestmod = multinom(data$Star_Type~.,data=data[,selected])   # Fits the best submodel
coef(bestmod)

