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
library(Hmisc)
library(corrplot)
library(dplyr)
library(nnet)
library(caret)
library(pls)
library(leaps)
library(RcmdrMisc)
library(heatmaply)
library(visNetwork)
library(ggpubr)
library(rstatix)

### -- I.2. DATASET --------------------------

# We open the table and convert it in data.table :
stars <- fread("./data/stars_by_Marie.csv",sep=",")
stars <- data.table(stars)

stars.V2 <- stars %>% dplyr::rename(temperature = Temperature.K, 
                             luminosite = Luminosity.L.Lo, 
                             rayon = Radius.R.Ro, 
                             magnitude = Absolute_Magnitude.Mv, 
                             couleur = Star_Color, 
                             spectre = Spectral_Class, 
                             type = Star_Type) %>% mutate(type = as.factor(type), 
                                                          couleur = as.factor(couleur), 
                                                          spectre= as.factor(spectre))

levels(stars.V2$type) <- c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche")

table <- stars.V2 |>
  skimr::skim() |>
  gt::gt() %>% as.data.frame() %>% select(2,7,8,9,11,13) %>% dplyr::rename(Variable = skim_variable,
                                                                    Effectifs = "factor.top_counts",
                                                                    Moyenne = numeric.mean,
                                                                    SD = numeric.sd,
                                                                    Q1 = numeric.p25,
                                                                    Q3 = numeric.p75)
