

###Data cleaning
data(possum)
dt <- possum


# data cleaning / processing
dt <- na.omit(dt, cols = "age")# removing observations without age
dt[,2:14]  # droping the case number
dt <- dt[,-(1:3)]

# stepwise regression
mod_null <- lm(age ~ 1, data=dt)
mod_global <- lm(age ~ ., data=dt)
forward <- step(mod_null, scope = formula(mod_global), direction = "forward", trace = 1)
forward$anova

# calculate RSS
### Rank the models according to their RSS (using package leaps)
select = summary(regsubsets(age~.,data=dt,nvmax=9))

### Boolean selection matrix for best submodels 
select$which

### RSS plot for exhaustive feature selection
plot(1:9,select$rss,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab="Residual sum-of-squares",main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
grid()


mod <- glm(age ~ ., data=dt)
cvmod<- boot::cv.glm(data=dt[,select$which[1,]],mod,K=10)

### Accuracy values for best submodels
acc = rep(0,9) # Initialize a vector of accuracy values
for (k in 1:9) {
  select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
  predictions = predict(select)
  acc[k] = mean(round(predictions)==dt$age)
} 
acc

### prediction error
error = rep(0,9)     # vector null of PRESS for best sub-models
for (j in 1:9) {
  mod = glm(age~.,data=dt[,select$which[j,]])
  cvmod = boot::cv.glm(dt[,select$which[j,]],mod,K=10)
  error[j] = cvmod$delta[2]*nrow(dt) 
}
error <- data.frame(error)



matplot(1:9, cbind(select$rss,error),type="l",col=c("red","green"),lty=c(1,1))
abline(330,0,col = "blue")

