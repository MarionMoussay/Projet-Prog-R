
# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Stars type",
             
             # theme css
             theme = "css/bootstrap.min.css",
             
             # premier onglet Data
             tabPanel("Data", 
                      navlistPanel(
                        widths = c(2, 10), 
                        tabPanel("Table", 
                                 # titre avec css
                                 h1("Jeu de données", style = "color : #0099ff;text-align:center"),
                                 # table
                                 dataTableOutput("table")),
                        tabPanel("Str",
                                 verbatimTextOutput("str")
                                 ),
                        tabPanel("Summary",
                                 verbatimTextOutput("summary")),
                      )
             ), 
             
             # second onglet Visualisation
             tabPanel("Visualisation", 
                      
                      fluidRow(
                        # premier colonne
                        column(width = 3, 
                               # wellPanel pour griser
                               wellPanel(
                                 # input pour la couleur
                                 colourInput(inputId = "color", label = "Couleur :", value = "orange"),
                                 
                                 # selection de la colonne
                                 radioButtons(inputId = "var", label = "Variable : ", choices = colnames(stars[,1:4])),
                                 
                                 # bouton
                                 actionButton("go", "GO!!!")
                               )
                        ), 
                        # deuxieme colonne
                        column(width = 9, 
                               tabsetPanel(id = "viz", 
                                 tabPanel("Distribution boxplot",
                                          amChartsOutput("distribution_boxplot1"),
                                          amChartsOutput("distribution_boxplot2"),
                                          amChartsOutput("distribution_boxplot3"),
                                          amChartsOutput("distribution_boxplot4")
                                          ),
                                 tabPanel("Geom bar count",
                                          plotOutput("count_type"),
                                          plotOutput("count_class"),
                                          plotOutput("count_color")
                                          ),
                                 tabPanel("Star type effect",
                                          amChartsOutput("star_type_boxplot1"),
                                          amChartsOutput("star_type_boxplot2"),
                                          amChartsOutput("star_type_boxplot3"),
                                          amChartsOutput("star_type_boxplot4")
                                          )
                               )
                        )
                      )
             ),
             tabPanel("Diagramme HR",
                      plotOutput("diagramme_HR1"),
                      plotOutput("diagramme_HR2"),
                      plotOutput("diagramme_HR3")
             ),
             tabPanel("ACP", 
                      
                      fluidRow(
                        # premier colonne
                        column(width = 3, 
                               # wellPanel pour griser
                               wellPanel(
                                 # input pour la couleur
                                 colourInput(inputId = "colorACP", label = "Couleur :", value = "blue"),
                                 colourInput(inputId = "colorACPsupp", label = "Couleur supplémentaire :", value = "orange"),
                                 
                                 numericInput("dim1", "Première dimension:", 1,
                                              min = 1, max = 4),
                                 
                                 numericInput("dim2", "Seconde dimension:", 2,
                                              min = 1, max = 4),

                                 # bouton
                                 actionButton("goACP", "GO!!!")
                               )
                        ), 
                        # deuxieme colonne
                        column(width = 9, 
                               tabsetPanel(id = "vizACP", 
                                           tabPanel("summary",
                                                    verbatimTextOutput("summaryACP")
                                           ),
                                           tabPanel("individus",
                                                    plotOutput("ACP_ind"),
                                                    plotOutput("ACP_ind_ellipse")
                                           ),
                                           tabPanel("variables",
                                                    plotOutput("ACP_var")
                                           ),
                                           tabPanel("valeurs_propres",
                                                    plotOutput("graph_vp"),
                                                    textOutput("text_vp")
                                           )
                               )
                        )
                      )
             ),
             
             
             # onglet sur la societe
             tabPanel("About",
                      # rajout d'une image avec img()
                      # elle doit etre dans www
                      img(src = "img/DATASTORM-GENES.jpg", width = 200),
                      tags$hr(),
                      "En tant que filiale du GENES, ", 
                      a(href = "www.datastorm.fr", "DATASTORM"), 
                      " valorise l’ensemble des 
                      activités de recherche du Groupe auprès des entreprises et administrations."
             )
  )
)