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
                        span(tags$i(h2("Etape 1 : choix de la distance ultramétrique")), style="color:#045a8d"),
                        sidebarPanel(
                          span(tags$i(h5("La distance ultramétrique définit la manière dont les individus sont regroupés à chaque itération de l'algorithme.")), style="color:#045a8d"),
                          span(tags$i(h5("La plus utilisée, parce que plus optimale, est la distance de Ward.")), style="color:#045a8d"),
                          uiOutput("choix_ultrametric"),
                          span(tags$i(h5("A droite s'affichent différentes fonctions de perte, elles vous permettent d'afficher le nombre de clusters jugé optimale selon chacune.")), style="color:#045a8d"),
                          span(tags$i(h5("Au regard de ces dernières, vous pouvez choisir un nombre k de clusters.")), style="color:#045a8d"),
                          
                        ),
                        mainPanel(
                          span(tags$i(h3("Fonctions de perte")), style="color:#045a8d"),
                          span(tags$i(h5("L'affichage des fonctions de perte peut prendre un peu de temps.")), style="color:#045a8d"),
                          tabsetPanel(
                            tabPanel("Inertie intra-groupe", plotlyOutput("fct_perte_coude")),
                            tabPanel("Statistiques de GAP", plotlyOutput("fct_perte_gap")),
                            tabPanel("Méthode silhouette", plotlyOutput("fct_perte_silhouette"))
                            
                          )
                        )
                        
                      ),
                      fluidRow(span(tags$i(h2("Etape 2 : choix du nombre de clusters")), style="color:#045a8d"),
                               sidebarPanel(
                                 sliderInput("nb_clusters", "Nombre de clusters souhaité :", 1, 10, 5,sep = 1),
                                 span(tags$i(h5("A droite s'affichent le dendogramme résultant de la classification et le 'HR-Diagram' selon les clusters.")), style="color:#045a8d"),
                                 
                                 
                               ),
                               mainPanel(
                                 column(6,
                                        tabsetPanel(
                                          tabPanel("Dendogramme", plotlyOutput("plot_dendogramme"))
                                        )
                                 ),
                                 column(6,
                                        tabsetPanel(
                                          tabPanel("'HR-Diagram'", plotlyOutput("hr_diag_clusters"))
                                        )
                                 ),
                                 
                               ))
                      
             )
  )
)
