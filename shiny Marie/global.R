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

### -- I.2. DATASET --------------------------

# We open the table and convert it in data.table :
stars <- fread("./data/stars_by_Marie.csv",sep=",")
stars <- data.table(stars)

stars$Star_Type <- as.factor(stars$Star_Type)
stars$Star_Color <- as.factor(stars$Star_Color)
stars$Spectral_Class <- as.factor(stars$Spectral_Class)

#stars