### ----- I. INTRODUCTION --------------------
### -- I.1. LIBRARY --------------------------

library(shiny)
library(rAmCharts)
library(colourpicker)
library(data.table)
library(DT)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(magick)
library(plotly)
library(shinyWidgets)
library(colourpicker)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(leaps)
library(nnet)


### -- I.2. DATASET --------------------------

# We open the table and convert it in data.table :
stars <- fread("./data/stars_by_Marie.csv",sep=",")
stars <- data.table(stars)

stars$Star_Type <- as.factor(stars$Star_Type)
stars$Star_Color <- as.factor(stars$Star_Color)
stars$Spectral_Class <- as.factor(stars$Spectral_Class)

#stars
library(nnet)
library(caret)

liste= c("Temperature.K","Luminosity.L.Lo","Radius.R.Ro")

data <- stars %>% select(1:5)

if (length(liste)== 0){
  mod <- multinom(Star_Type~1, data=data)
} else {
  var <- c(liste[1])
  for (i in 2:(length(liste))){
    var <- paste0(var, paste0("+", liste[i]))
  }
  formul <- paste0("Star_Type~",var )
  mod <- multinom(as.formula(formul), data=data)
  
}
mod

confusionMatrix(predict(mod, newdata = data), data$Star_Type)
table(predict(mod, newdata = data), data$Star_Type)
