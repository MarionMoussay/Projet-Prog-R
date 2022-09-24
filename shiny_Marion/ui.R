library(shiny)
library(rAmCharts)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(factoextra)

# Define UI for application that draws a histogram
shinyUI(
    # navbarPage
    navbarPage("Premiers pas avec shiny",
               
               # premier onglet Data
               tabPanel("Data", 
                        navlistPanel(
                            tabPanel("Table",
                                     dataTableOutput("table")),
                            tabPanel("Summary",
                                     verbatimTextOutput("summary"))
                        )
                        
                        
                        
               ), 
               
               # second onglet Visualisation
               tabPanel("CAH", 
                        fluidRow( 
                            sidebarPanel(
                                     uiOutput("choix_ultrametric")
                                   
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Inertie intra-groupe", plotlyOutput("fct_perte_coude")),
                                tabPanel("Statistiques de GAP", plotlyOutput("fct_perte_gap")),
                                tabPanel("Méthode silhouette", plotlyOutput("fct_perte_silhouette"))
                                
                              )
                            )
                            
                        ),
                        fluidRow()
                        
               )
    )
)
