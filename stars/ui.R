library(shiny)
library(colourpicker)
library(rAmCharts)

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
                            column(width = 3, 
                                   sliderInput("bins",
                                               "Number of bins:",
                                               min = 1,
                                               max = 50,
                                               value = 30),
                            ), 
                            column(width=3, 
                                   # input pour la couleur
                                   colourInput(inputId = "color", label = "Couleur :", value = "purple")
                            ),
                            column(width=3, 
                                   # titre du graphique
                                   textInput(inputId = "titre", label = "Titre :", value = "Données")
                            ),
                            column(width=3, 
                                   # selection de la colonne
                                   radioButtons(inputId = "var", label = "Variable : ", choices = colnames(faithful)),
                                   actionButton(inputId = "button", label="Refresh", icon=icon("thumbs-up"))
                            ),
                        ),
                        fluidRow(tabsetPanel(
                            tabPanel("Histogramme",
                                     amChartsOutput("histogramme"),
                                     # classes (div centrée)
                                     div(textOutput("n_bins"), align = "center"))),
                            tabPanel("Boxplot",
                                     amChartsOutput("boxplot"))
                        )
                        
               )
    )
)
