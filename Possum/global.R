####################################################
####################################################
############### GLOBAL ENVIRONMENT #################

#load library
require(shiny)
require(DAAG)                
require(shinyWidgets)
require(shinydashboard)
library(data.table)
# library(DT)
library(rpart)  # decision tree
library(visNetwork)  # decision tree
library(sparkline)  # decision tree
library(ggplot2)
library(plotly)
require(leaps)  # regsubsets function
require(RcmdrMisc)  # stepwise function
require(boot)  # For cv.glm
library(shinyFeedback)
library(shinycssloaders)

# load data dt
data(possum)
dt <- possum


# data cleaning / processing
dt <- na.omit(dt, cols = "age")  # removing observations without age
dt <- dt[,2:14]  # droping the case number

# dt[, case := NULL]  # droping the case number with data.table
# dt[, c("site", "Pop", "sex") := lapply(.SD, as.factor), .SDcols = c("site", "Pop", "sex")]
cols <- c("site","Pop","sex")
dt[cols] <- lapply(dt[cols],as.factor)


# load functions
source("Function.R")

#dataset for correlation heatmap
dt.quanti <- dt[,-(1:3)]
x <- as.matrix(dt.quanti)
mat_corr <- cor(na.omit(dt.quanti))


# calculate RSS
### Rank the models according to their RSS (using package leaps)
select = summary(regsubsets(age~.,data=dt.quanti,nvmax=9))

### Boolean selection matrix for best submodels 
select$which
mod <- glm(age ~ ., data=dt.quanti)

###AIC/BIC for best submodels
var_aic <- rep(0,9)# vector null of AIC for best sub-models
var_bic <- rep(0,9)

for (i in 1:9) {
  mod <- glm(age~.,data=dt.quanti[,select$which[i,]])
  var_aic[i] <- AIC(mod)
  var_bic[i] <- BIC(mod)
}


# cvmod<- boot::cv.glm(data=dt[,select$which[1,]],mod,K=10)

### Accuracy values for best submodels
# acc = rep(0,9) # Initialize a vector of accuracy values
# for (k in 1:9) {
#   select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
#   predictions = predict(select)
#   acc[k] = mean(round(predictions)==dt$age)
# } 
# acc

### prediction error
error = rep(0,9)     # vector null of PRESS for best sub-models
for (j in 1:9) {
  mod = glm(age~.,data=dt.quanti[,select$which[j,]])
  cvmod = boot::cv.glm(dt.quanti[,select$which[j,]],mod,K=10)
  error[j] = cvmod$delta[2]*nrow(dt.quanti) 
}
error <- data.frame(error)

error$error


# mod2 <- lm(age~belly+hdlngth, data = dt.quanti)
# mod3 <- lm(age~eye+chest+belly, data = dt.quanti)
# anova(mod2,mod3)