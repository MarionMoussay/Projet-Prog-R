
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
                       
                       ## Magnitude absolue : luminosité intrasèque d'un astre (à contrario de la magnitude apparente qui correspond à une mesure d'irradiance)
                       
                       
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
                          
                          ## RESUMES  
                          
                          tabPanel("Résumés", 
                                   h2("Résumé du tableau"),
                                   dataTableOutput("str"),
                                   h2("_______________________________________"),
                                   h2("Résumé statistique"),
                                   dataTableOutput("summary"),
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
                          
                          tabPanel("Liaison entre les variables", 
                                   verticalLayout(
                                      
                                      ## CORRELOGRAMME ET TEST DE CORRELATION
                                      sidebarLayout(
                                         sidebarPanel(width = 6,
                                                      h2("Entre les variables numériques :"),
                                                      fluidRow(
                                                         column(width = 4,verbatimTextOutput("corr_result")),
                                                         column(width = 8,plotlyOutput("graph_corr", height = "400px" ))
                                                      ),
                                         ),
                                         mainPanel(width = 4,
                                                   ## KHI-DEUX
                                                   h2("Entre les variables catégorielles :"),
                                                   verbatimTextOutput("khi2"),
                                                   h4("L'hypothèse d'indépendance entre les deux variables est rejetée."),
                                         ),
                                      ), 
                                      
                                      h2("Analyse de la variance"), 
                                      
                                      sidebarLayout(
                                         sidebarPanel(width=4,
                                                      awesomeRadio(
                                                         inputId = "choix_var_anova",
                                                         label = "Choisissez la variable numérique qui composera le modèle anova :", 
                                                         choices = list("Température"="temperature", "Luminosité" ="luminosite", "Rayon"="rayon", "Magnitude"="magnitude")
                                                      ),
                                                      verbatimTextOutput("summary_anova")
                                                ),
                                         mainPanel(width=8,
                                                verticalLayout(
                                                   h4("Normalité des résidus"),
                                                   plotlyOutput("qqplot"),
                                                   dataTableOutput("shapiro"),
                                                   br()
                                                )
                                                )
                                      )
                                   ),
                          ),
                          
                          ##### ANALYSE STRUCTURE #####
                          
                          tabPanel("Analyse de la structure",
                                   
                                   ## ACP
                                   
                                   h2("Analyse en composantes principales : analyse de la structure du jeu de données"),
                                   fluidRow(
                                      column(width = 3,
                                             wellPanel(
                                                colourpicker::colourInput(inputId = "colorACP", label = "Couleur :", value = "#BF1B2E"),
                                                colourpicker::colourInput(inputId = "colorACPsupp", label = "Couleur supplémentaire :", value = "#4C79B5"),
                                                
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
                                                         
                                                         tabPanel("Graphe des individus",
                                                                  plotlyOutput("ACP_ind"),
                                                                  plotlyOutput("ACP_ind_ellipse")
                                                         ),
                                                         tabPanel("Graphes des variables",
                                                                  plotlyOutput("ACP_var")
                                                         ),
                                                         tabPanel("Variance expliquée",
                                                                  plotlyOutput("graph_vp"),
                                                                  textOutput("text_vp")
                                                         ),
                                                         tabPanel("Résumé",
                                                                  verbatimTextOutput("summaryACP")
                                                         )
                                             )
                                      )
                                   ),
                                   br(),
                                   br(),
                                   
                                   
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
                                                                                                  choices = c("Température"="temperature", "Luminosité"="luminosite", "Angle"="angle", "Magnitude absolue"="magnitude"))),
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
                                  h2("Arbre de décision CART"),
                                  h3("L'objective est ici de créer un arbre de décison CART à partir des différentes caractéristiques des étoiles et de prédire avec ce modèle le type d'une nouvelle étoile."),
                                  
                                  sidebarLayout(
                                     sidebarPanel(
                                        sliderInput("nb_ech_app", "Taille de l'échantillon d'apprentissage :", 120, 240, 180,sep = 1),
                                        visNetworkOutput("arbreCART", height = '700px')
                                     ),
                                     mainPanel( 
                                        verticalLayout(fluid=TRUE, 
                                                       h4("__________________________________________________________________"),
                                                       h4("Matrice de confusion"),
                                                       verbatimTextOutput("pred_CART"),
                                                       verbatimTextOutput("accuracy"),
                                                       h4("__________________________________________________________________"),
                                                       h4("Taux de mauvais classement en fonction de la taille de l’arbre"),
                                                       plotOutput("cp")
                                        )
                                        
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






