
# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Classification des étoiles", theme = shinytheme("cosmo"),
             tabPanel("Contexte"
             ),
             tabPanel("Jeux de données", 
                      # titre avec css
                      h1("Jeu de données", style = "color : #0099ff;text-align:center"),
                      # table
                      dataTableOutput("table")
             ),
             navbarMenu("Statistiques descriptives",
                        # premier onglet Data
                        tabPanel("Vue globale",
                                 navlistPanel(
                                   widths = c(2, 10), 
                                   tabPanel("Str",
                                            verbatimTextOutput("str")
                                   ),
                                   tabPanel("Summary",
                                            verbatimTextOutput("summary")),
                                 )), 
                        tabPanel("Diagramme HR",
                                 htmlOutput('imgf'),
                                 verbatimTextOutput("summaryHR"),
                                 plotlyOutput("diagramme_HR1"),
                                 plotlyOutput("diagramme_HR2"),
                                 plotlyOutput("diagramme_HR3")), 
                        tabPanel("ACP",
                                 fluidRow(
                                   # premier colonne
                                   column(width = 3,
                                          # wellPanel pour griser
                                          wellPanel(
                                            # input pour la couleur
                                            colourpicker::colourInput(inputId = "colorACP", label = "Couleur :", value = "blue"),
                                            colourpicker::colourInput(inputId = "colorACPsupp", label = "Couleur supplémentaire :", value = "orange"),
                                            
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
                                                               plotlyOutput("ACP_ind"),
                                                               plotlyOutput("ACP_ind_ellipse")
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
                        tabPanel("Boxplot", 
                                 fluidRow(
                                   # premier colonne
                                   column(width = 3,
                                          # wellPanel pour griser
                                          wellPanel(
                                            # input pour la couleur
                                            colourpicker::colourInput(inputId = "color", label = "Couleur :", value = "orange"),
                                            
                                            # selection de la colonne
                                            uiOutput("choix_var_graph"),
                                            
                                            # bouton
                                            actionButton("go", "GO!!!")
                                          )
                                   ),
                                   # deuxieme colonne
                                   column(width = 9,
                                          tabsetPanel(id = "viz",
                                                      tabPanel("Distribution boxplot",
                                                               amChartsOutput("distribution_boxplot1")
                                                               # amChartsOutput("distribution_boxplot2"),
                                                               # amChartsOutput("distribution_boxplot3"),
                                                               # amChartsOutput("distribution_boxplot4")
                                                      ),
                                                      tabPanel("Geom bar count",
                                                               plotOutput("count_type"),
                                                               plotOutput("count_class"),
                                                               plotOutput("count_color")
                                                      ),
                                                      tabPanel("Star type effect",
                                                               #plotOutput("star_type_boxplot1"),
                                                               amChartsOutput("star_type_boxplot1"),
                                                               amChartsOutput("star_type_boxplot2"),
                                                               amChartsOutput("star_type_boxplot3"),
                                                               amChartsOutput("star_type_boxplot4")
                                                      )
                                          )
                                   )
                                 )
                                 
                        )
                        
             ),
             tabPanel("Modèle prédictif"), 
             tabPanel("Classification Ascendante Hiérarchique", 
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
