
# Define UI for application that draws a histogram
fluidPage(
   # shinythemes::themeSelector(),
   # navbarPage
   navbarPage("Classification des étoiles", 
              theme = shinytheme("sandstone"),
              ########## ONGLET 1 ###############
              tabPanel("Contexte", 
                       h1("Le diagramme de Hertzsprung-Russell, la réference officielle de classification des étoiles"),
                       tabsetPanel(id="onglet1",
                                   tabPanel("En théorie",
                                            sidebarLayout(fluid = TRUE,
                                                          sidebarPanel(includeHTML("include.html"), 
                                                                       icon("fa-thin fa-star"),
                                                                       HTML('<a href="https://en.wikipedia.org/wiki/Hertzsprung%E2%80%93Russell_diagram" role="button">Pour en savoir plus</a>')), 
                                                          mainPanel(img(src = "diagramme-hr.jpg", width="700", height="700", position="center")), 
                                                          
                                            )), 
                                   tabPanel("En pratique",
                                            sidebarLayout(
                                               sidebarPanel(width = 4, 
                                                            h4("En utilisant notre jeu de données, on peut réaliser le diagramme HR."),
                                                            uiOutput("choix_var_hrdiag")), 
                                               mainPanel(width = 8,
                                                         plotlyOutput("diagramme_HR1"), 
                                                         plotlyOutput("diagramme_HR2"), 
                                                         plotlyOutput("diagramme_HR3")))
                                   )
                       )
                       
              ),
              ########## ONGLET 2 ###############
              tabPanel("Jeux de données", 
                       # titre avec css
                       h1("Jeu de données", style = "color : #0099ff;text-align:center"),
                       # table
                       dataTableOutput("table"), 
                       downloadButton("downloadCsv2", "Télécharger"),tags$br(),tags$br()
              ),
              ########## ONGLET 3 ###############
              navbarMenu("Statistiques descriptives",
                         # premier onglet Data
                         tabPanel("Vue globale",
                                  tabsetPanel(
                                     tabPanel("Résumés",
                                              h4("Résumé du tableau"),
                                              verbatimTextOutput("str"),
                                              h4("Résumé statistique"),
                                              verbatimTextOutput("summary")
                                     ),
                                     tabPanel("Effectifs",
                                              
                                              
                                              plotlyOutput("count_type"),
                                              plotlyOutput("count_class"),
                                              plotlyOutput("count_color")
                                     )
                                  )),
                         
                         tabPanel("Boxplot", 
                                  fluidRow(
                                     # premier colonne
                                     column(width = 3,
                                            # wellPanel pour griser
                                            wellPanel(
                                               # input pour la couleur
                                               colourpicker::colourInput(inputId = "color", label = "Couleur :", value = "orange"),
                                               
                                               # selection de la colonne
                                               awesomeRadio(
                                                  inputId = "choix_var_graph",
                                                  label = "Choisissez la variable à illuster :", 
                                                  choices = list("Température"="Temperature.K", "Luminosité" ="Luminosity.L.Lo", "Rayon" ="Radius.R.Ro", "Magnitude"="Absolute_Magnitude.Mv")
                                               ),
                                               
                                               # bouton
                                               actionButton("go", "VALIDER")
                                            )
                                     ),
                                     # deuxieme colonne
                                     column(width = 9,
                                            tabsetPanel(id = "viz",
                                                        tabPanel("Distribution par caractéristiques",
                                                                 amChartsOutput("distribution_boxplot1")
                                                        ),
                                                        
                                                        tabPanel("Distribution par type des étoiles",
                                                                 amChartsOutput("star_type_boxplot1")
                                                        )
                                            )
                                     )
                                  )
                                  
                         ),
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
                                               actionButton("goACP", "VALIDER")
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
                                                                 plotlyOutput("ACP_var")
                                                        ),
                                                        tabPanel("valeurs propres",
                                                                 plotlyOutput("graph_vp"),
                                                                 textOutput("text_vp")
                                                        )
                                            )
                                     )
                                  )
                                  
                         )
                         
              ),
              ########## ONGLET 4 ###############
              tabPanel("Modèle prédictif", 
                       h1("Le diagramme de Hertzsprung-Russell, la réference officielle de classification des étoiles"),
                       tabsetPanel(id="onglet1",
                                   tabPanel("Modèle multinomiale",
                                            sidebarLayout(fluid = TRUE,
                                                          sidebarPanel(checkboxGroupInput(inputId = "choix_var_mod_mult", label = "Choisissez le ou les variables à utiliser dans le modèle", 
                                                                                          choices = c("Temperature (K)"="Temperature.K", "Luminosité (L.lo)"="Luminosity.L.Lo", "Radius (R.ro)"="Radius.R.Ro", "Magnitude Absolue"="Absolute_Magnitude.Mv"))), 
                                                          mainPanel(
                                                             tabsetPanel(tabPanel("Sommaire du modèle",verbatimTextOutput("resum_mod")), 
                                                                         tabPanel("Plot", 
                                                                                  #courbe de régression
                                                                                  ), 
                                                                         tabPanel("Valeurs prédites",  
                                                                                  #predict(fff, newdata=stars) + matrice de confusion)
                                                                                  ), 
                                                          )
                                                             
                                            )), 
                                   tabPanel("Ré-échantillonnage leave-one-out",
                                            sidebarLayout(
                                               sidebarPanel(), 
                                               mainPanel())
                                   )
                       )
                       
                       
                       
              ), 
              
              ########## ONGLET 5 ###############
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
              ),
              div(style = "margin-bottom: 30px;"),
              
              tags$footer(column(6, "Institut Agro-campus, Rennes", icon = icon("fa-sharp fa-solid fa-house")), 
                          column(2, actionLink("twitter_share", label = "Share", icon = icon("fa-thin fa-star"),
                                               style= "color:white;", onclick = sprintf("window.open('%s')", 
                                                                                        "https://github.com/MarionMoussay/Projet-Prog-R"))),
                          column(2, actionLink("easter_egg", label = "Ne pas cliquer", icon = icon("stop"),
                                               style= "color:white;", onclick = sprintf("window.open('%s')", 
                                                                                        "https://www.youtube.com/watch?v=dty7JyWKoKg"))),
                          style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #25292C"
              ) 
              
   ))
