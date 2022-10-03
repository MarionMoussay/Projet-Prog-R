
## Fil conducteur : 

## ONGLET 1 - CONTEXTE : Quel est le contexte de recherche / enjeux 
## ONGLET 2 - FOCUS SUR LES DONNEES : 
## Comment les variables sont-elles distribués selon le type de l'étoile, quelle est la structure du jeu de données ?
##    - Jeu de données brut;
##    - Résumés (tableau et statistique);
##    - Statistiques descriptives (visualisation des variables quantitatives et qualitatives en fonction du type d'étoiles);
##    - Analyse de la structure du jeu de données : corrélations + ACP 
## ONGLET 3 - CLASSIFICATION DES ETOILES : comment le type d'étoiles est définit ?
##    - Classification officielle : diagramme HR
##    - Modèles de prédictions : modèle de régression (l'objectif étant de prédire un type d'étoile pour des nouvelles entrées de variable)
##    - Arbre de décision CART
##    - Classification Ascendante Hiérarchique : comparaison des groupes de sortie 



fluidPage(
   # shinythemes::themeSelector(),
   # navbarPage
   navbarPage("Caractérisation des étoiles", 
              theme = shinytheme("sandstone"),
              
              ########## ONGLET 1 ###############
              
              tabPanel("Contexte",
                       
                       
                       
                       
              ),
              
              ## JEU DE DONNEES BRUT
              
              tabPanel("Jeu de données",
                       tabsetPanel(
                          ## TELECHARGER CSV
                          
                          tabPanel("Télécharger les données", 
                                   h1("Jeu de données"),
                                   h4("Le jeu de données comportent 40 étoiles de chaque type."),
                                   dataTableOutput("table"),
                                   downloadButton("downloadCsv2", "Télécharger"),tags$br(),tags$br(),
                                   br(),
                                   br()
                          ),
                          
                          ## DISTRIBUTIONS GLOBALES
                          
                          tabPanel("Distribution des variables",
                                   
                                   verticalLayout(fluid=TRUE,
                                                  fluidRow(
                                                     
                                                     ## BOXPLOT VARIABLES QUANTITATIF ##
                                                     
                                                     h2("Distribution des caractéristiques numériques"),
                                                     column(width = 3,
                                                            wellPanel(
                                                               h4("Le but est ici de visualiser la distribution de chaque variable quantitive."),
                                                               awesomeRadio(
                                                                  inputId = "choix_var_boxplot",
                                                                  label = "Choisissez la variable à illuster :", 
                                                                  choices = list("Température"="temperature", "Luminosité" ="luminosite", "Rayon" ="rayon", "Magnitude"="magnitude")
                                                               ),
                                                               
                                                            )
                                                     ),
                                                     column(width = 9,amChartsOutput("star_boxplot"))
                                                  ), 
                                                  fluidRow(
                                                     
                                                     ## GRAPHES BATONS VARIABLES QUALITATIVES ##
                                                     
                                                     h2("Effectifs des caractéristiques catégorielles"),
                                                     column(width = 3,
                                                            wellPanel(
                                                               h4("Le but est ici de visualiser les effectifs de chaque variable qualitative."),
                                                               awesomeRadio(
                                                                  inputId = "choix_var_quali",
                                                                  label = "Choisissez la variable à illuster :", 
                                                                  choices = list("Classe spectrale"="spectre", "Couleur" ="couleur")
                                                               ),
                                                               
                                                            )
                                                     ),
                                                     column(width = 9, plotlyOutput("histo_quali"))
                                                  ),
                                                  br(),
                                                  br()
                                   )
                          ),
                          
                          ## RESUMES  
                          
                          tabPanel("Résumés", 
                                   h4("Résumé du tableau"),
                                   verbatimTextOutput("str"),
                                   h4("Résumé statistique"),
                                   verbatimTextOutput("summary"),
                                   br(),
                                   br()
                                   
                          ),
                          
                       )
                       
              ),
              
              ########## ONGLET 2 ###############
              
              # L'objectif est ici de mieux visualiser les données selon le type de l'étoile
              
              tabPanel("Focus sur les caractéristiques des étoiles", 
                       h1("Objectif : caractérisation des variables en fonction du type"),
                       
                       tabsetPanel(
                          
                          ## STATISTIQUES DESCRIPTIVES 
                          
                          tabPanel("Statistiques descriptives",
                                   
                                   verticalLayout(fluid=TRUE,
                                                  fluidRow(
                                                     
                                                     ## BOXPLOT PAR TYPE D'ETOILES ##
                                                     
                                                     h2("Distribution des caractéristiques numériques selon les types d'étoiles"),
                                                     column(width = 3,
                                                            wellPanel(
                                                               h4("Le but est ici de visualiser la distribution de chaque variable quantitive en fonction du type de l'étoile."),
                                                               awesomeRadio(
                                                                  inputId = "choix_var_boxplot_type",
                                                                  label = "Choisissez la variable à illuster :", 
                                                                  choices = list("Température"="temperature", "Luminosité" ="luminosite", "Rayon" ="rayon", "Magnitude"="magnitude")
                                                               ),
                                                               
                                                            )
                                                     ),
                                                     column(width = 9,amChartsOutput("star_type_boxplot"))
                                                  ), 
                                                  fluidRow(
                                                     
                                                     ## GRAPHES BATONS PAR TYPE D'ETOILES ##
                                                     
                                                     h2("Effectifs des caractéristiques catégorielles selon les types d'étoiles"),
                                                     column(width = 3,
                                                            wellPanel(
                                                               h4("Le but est ici de visualiser les effectifs de chaque variable qualitative selon les types des étoiles."),
                                                               awesomeRadio(
                                                                  inputId = "choix_var_quali_type",
                                                                  label = "Choisissez la variable à illuster :", 
                                                                  choices = list("Classe spectrale"="spectre", "Couleur" ="couleur")
                                                               ),
                                                               
                                                            )
                                                     ),
                                                     column(width = 9, plotlyOutput("histo_quali_type"))
                                                     
                                                  ),
                                                  br(),
                                                  br(),
                                   ), 
                          ),
                          ##### ANALYSE STRUCTURE #####
                          
                          tabPanel("Analyse de la structure",
                                   
                                   verticalLayout(fluid = TRUE, 
                                                  
                                                  ## CORRELOGRAMME ET TEST DE CORRELATION
                                                  
                                                  h2("Visualisation des corrélations"),
                                                  fluidRow(
                                                     column(width = 4,verbatimTextOutput("corr_result")),
                                                     column(width = 8,plotOutput("graph_corr", height = "400px" ))
                                                  ),
                                                  
                                                  ## ACP
                                                  
                                                  h2("Analyse en composantes principales : analyse de la structure du jeu de données"),
                                                  fluidRow(
                                                     column(width = 3,
                                                            wellPanel(
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
                                                  ),
                                                  br(),
                                                  br(),
                                   ),
                                   
                          )
                       ),
              ),
              
              ########## ONGLET 3 ###############
              
              navbarMenu("Classification des étoiles",
                         
                         ##### DIAGRAMME HR ####
                         
                         tabPanel("Classification officielle des étoiles",
                                  
                                  ### Objectif : poser le contexte de la classification d'étoiles, présenter les différents types d'étoiles et les variables caractérisant les groupes
                                  ### -> Mise en parallèle direct de la théorie versus les données 
                                  ### Toutes les informations sur un seul diagramme :
                                  ### - taille des points = luminosité, 
                                  ### - forme des points = classes spectrales, 
                                  ### - couleur = type d'étoiles, 
                                  ### - abcisses = température 
                                  ### - ordonnées = magnitude 
                                  
                                  h1("Le diagramme de Hertzsprung-Russell, la réference officielle de classification des étoiles"),
                                  h2("Objectif : définir le type d'une étoile en fonction de ses caractéristiques."),
                                  column(7,
                                         wellPanel(    
                                            verticalLayout(fluid = TRUE,
                                                           fluidRow(
                                                              column(4,includeHTML("contexteP1.html")), 
                                                              column(4,includeHTML("contexteP2.html")),
                                                              column(4,
                                                                     checkboxGroupInput(inputId = "choix_var_hrdiag", label = "Choisissez le ou les type(s) à représenter", 
                                                                                        choices = c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche"), 
                                                                                        selected = c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche")),
                                                                     icon("fa-thin fa-star"),
                                                                     HTML('<a href="https://en.wikipedia.org/wiki/Hertzsprung%E2%80%93Russell_diagram" role="button">Pour en savoir plus</a>')
                                                              )
                                                           ),
                                                           plotOutput("diagramme_HR", height = "500px")
                                            )
                                         )
                                  ),
                                  column(5, 
                                         img(src = "diagramme-hr.jpg", width="700", height="700", position="center"),
                                         
                                  ),
                         ),
                         
                  
                         
                         ## MODELE DE PREDICTION #####
                         
                         tabPanel("Modèle prédictif", 
                                  verticalLayout(fluid = TRUE, 
                                                 
                                                 ## Modèle multinomiale
                                                 
                                                 ## Objectif : permettre dans un premier temps à l'utilisateur de choisir
                                                 ##   - les variables pour construire le modèle multinomiale;
                                                 ##   - la métrique de validation.
                                                 ## -> ressort le summary, les valeurs prédites + matrices de confusion + critères 
                                                 
                                                 h4("Modèle multinomiale"),
                                                 fluidRow(
                                                    sidebarLayout(fluid = TRUE,
                                                                  sidebarPanel(checkboxGroupInput(inputId = "choix_var_mod_mult", label = "Choisissez le ou les variables à utiliser dans le modèle",
                                                                                                  choices = c("Temperature (K)"="Temperature.K", "Luminosité (L.lo)"="Luminosity.L.Lo", "Radius (R.ro)"="Radius.R.Ro", "Magnitude Absolue"="Absolute_Magnitude.Mv"))),
                                                                  mainPanel(
                                                                     tabsetPanel(tabPanel("Sommaire du modèle",verbatimTextOutput("resum_mod")),
                                                                                 tabPanel("Matrice de confusion",
                                                                                          verbatimTextOutput("pred") 
                                                                                 )
                                                                     ),
                                                                  )
                                                    )
                                                 ),
                                                 
                                                 ## Modèle d'apprentissage 
                                                 
                                                 h4("Apprentissage leave one-out"),
                                                 fluidRow(
                                                    sidebarLayout(fluid = TRUE,
                                                                  sidebarPanel(checkboxGroupInput(inputId = "choix_bf", label = "Choisissez le sens de l'algorithme",
                                                                                                  choices = c("forward","backward","forward/backward"))),
                                                                  mainPanel(
                                                                     tabsetPanel(tabPanel("Sommaire du modèle loocv",verbatimTextOutput("resum_loocv"))
                                                                     ),
                                                                  )
                                                    )
                                                 )
                                  )
                                  
                         ),
                         
                         ## ARBRE DE DECISION CART
                         
                         tabPanel("Arbre de décision CART",
                                  sidebarLayout(
                                     sidebarPanel(
                                        h3("Decision Tree"),
                                        helpText(
                                           "These controls are for setting the hyperparameter values",
                                           "which partly control the structure of the decision tree.",
                                           "The default values we've put in should create a fairly safe",
                                           "tree but try changing them if you're feeling adventurous."
                                        ),
                                        br(),
                                        h4("Minimum Split"),
                                        helpText(
                                           "If at a given node N is below this value, that node cannot",
                                           "be split any further: it is a terminal node of the tree."
                                        ),
                                        sliderInput(
                                           inputId = "min_split",
                                           label = NULL,  # label given in outer code
                                           min = 2,       # two is the smallest that could be split
                                           max = 10,      # chosen to not make the models too wild
                                           value = 2      # defaults to not having an artifical minimum
                                        ),
                                        br(),
                                        h4("Minimum Bucket Size"),
                                        helpText(
                                           "If creating a given split would cause N₁ or N₂ to fall below",
                                           "this minimum, then that split isn't made part of the",
                                           "decision tree."
                                        ),
                                        sliderInput(
                                           inputId = "min_bucket",
                                           label = NULL,  # label given in outer code
                                           min = 1,       # can't have buckets of size zero
                                           max = 30,      # rpart default is minbucket = 3*minsplit
                                           value = 1      # defaults to not having an artifical minimum
                                        ),
                                        br(),
                                        h4("Maximum Tree Depth"),
                                        helpText(
                                           "Control the maximum depth that the decision tree can reach.",
                                           "Note that, depending on what features are being used and the",
                                           "values of the other parameters, you may end up with a tree",
                                           "much shallower than the maximum."
                                        ),
                                        sliderInput(
                                           inputId = "max_depth",
                                           label = NULL,  # label given in outer code
                                           min = 2,       # a min of 2 allows for at least one split
                                           max = 30,      # rpart can't do 31+ depth on 32-bit machines
                                           value = 5      # chosen to not make the default too wild
                                        )
                                     ),
                                     mainPanel(
                                        plotOutput(outputId = "tree_plot")
                                     )
                                  )
                         ),
                         
                         ## CLASSIFICATION ASCENDANTE HIERARCHIQUE
                         
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
                                     ,
                                     br(),
                                     br()
                                  )
                         )
                         
              ),
              tabPanel("A propos de nous"),
              
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
              
   )
)






