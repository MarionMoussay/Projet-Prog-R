# Ci-joint notre plan. Les titres sont mis de sorte qu'avec (Top Level) il est simple 
# ... de naviguer à la partie souhaitée. Les titres sont identiques dans l'UI que dans 
# ... le SERVER, pour permettre de rapidement faire le lien entre la partie UI et la 
# ... partie SERVER associée.


# ONGLET 1  : CONTEXTE (Quel est le contexte de recherche / enjeux)
# ONGLET 2 : JEU DE DONNEES
#     1) Télécharger les données
#     2) Résumés
#     3) Distribution des variables
# ONGLET 3 : FOCUS SUR LES CARACTERISTIQUES DES ETOILES (Comment les variables sont-elles 
# ... distribués selon le type de l'étoile, quelle est la structure du jeu de données ?)
#     1) Statistiques descriptives
#     2) Liaison entre les variables
#     3) Analyse de la structure 
#           3.a) Graphe des individus
#           3.b) Graphe des variables
#           3.c) Variance expliquée
#           3.d) Résumé
# ONGLET 4  : CLASSIFICATION DES ETOILES (comment le type d'étoiles est définit ?)
# ONGLET 4.1  : CLASSIFICATION OFFICIELLE DES ETOILES
# ONGLET 4.2  : MODELE PREDICTIF
#     1) Prédire une nouvelle étoile (l'objectif étant de prédire un type d'étoile pour des nouvelles entrées de variable)
#     2) Choix du modèle
#           2.a) Sommaire du modèle
#           2.b) Matrice de confusion
#           2.c) Recherche du meilleur modèle au sens de l'AIC et BIC
# ONGLET 4.3  : ARBRE DE DECISION
# ONGLET 4.4  : CLASSIFICATION ASCENDANTE HIERARCHIQUE (comparaison des groupes de sortie)
#     1) Inertie intra-groupe
#     2) Statistiques de Gap
#     3) Méthode silhouette
#     4) Dendogramme
#     5) Diagramme HR




fluidPage(
   # shinythemes::themeSelector(),
   # navbarPage
   navbarPage("Caractérisation du type des étoiles", 
              theme = shinytheme("sandstone"),
              
              ############ ---- ONGLET 1 : CONTEXTE --------------------
              
              tabPanel("Contexte",
                       br(),
                       sidebarLayout(
                          sidebarPanel(width=12,
                                       h1("Pourquoi classifier les étoiles?", align = "center"),
                                       h3("- Avec l'émergence de nouvelles technologies de pointes, l'analyse des données constitue un point essentiel pour de nombreux domaines tels que la biologie, la cosmologie ou l'astrophysique."),
                                       h3("- Grâce à des données morphologiques où à des paramètres liés à la structure de l'objet et à la photométrie, il est possible de séparer les étoiles en différents groupes.", em("(Bertin & Arnouts 1996, Henrion et al. 2011, Molino et al. 2014, Diaz-García et al. 2019, Lopez-Sanjuan et al. 2019)")),
                                       br(),
                                       h3("L'objectif de notre application est ainsi de pouvoir déterminer l'appartenance d'une étoile à un groupe selon plusieurs modalités."),
                                       h3("Pour cela, nous disposons d'un jeu de données contenant 240 observations d'étoiles décrites selon 6 variables qui sont la température (en Kelvin), la luminosité relative au Soleil, le rayon relatif au Soleil, la magnitude absolue, la couleur et la classe spectrale."),
                                       h3("Voici une illustration de l'apparence des différents types d'étoiles:"),
                                       br(),
                                       HTML('<center><img src="type_stars.jpg", style= "border-radius: 10% 10% 10% 10%; border: 3px solid"></center>'),
                          ),
                          mainPanel(width=0,
                          )
                       ),
              ),
              
              
              ############ ---- ONGLET 2 : JEU DE DONNEES --------------------
              
              tabPanel("Jeu de données",
                       tabsetPanel(
                          
                          #### 1) Télécharger les données #####################
                          
                          tabPanel("Télécharger les données", 
                                   h1("Jeu de données"),
                                   h4("Le jeu de données comporte 40 étoiles de chaque type."),
                                   dataTableOutput("table"),
                                   downloadButton("downloadCsv2", "Télécharger"),tags$br(),tags$br(),
                                   br(),
                                   br()
                          ),
                          
                          #### 2) Résumés ###############################
                          
                          tabPanel("Résumés", 
                                   h2("Résumé du tableau"),
                                   dataTableOutput("str"),
                                   h2("_______________________________________"),
                                   h2("Résumé statistique"),
                                   dataTableOutput("summary"),
                                   br(),
                                   br()
                                   
                          ),
                          
                          #### 3) Distribution des variables ###############################
                          
                          tabPanel("Distribution des variables",
                                   
                                   verticalLayout(fluid=TRUE,
                                                  fluidRow(
                                                     
                                                     # Distribution des caractéristiques numériques :
                                                     
                                                     h2("Distribution des caractéristiques numériques"),
                                                     column(width = 3,
                                                            wellPanel(
                                                               h4("Le but est ici de visualiser la distribution de chaque variable quantitive."),
                                                               awesomeRadio(
                                                                  inputId = "choix_var_boxplot",
                                                                  label = "Choisissez la variable à illustrer :", 
                                                                  choices = list("Température"="temperature", "Luminosité" ="luminosite", "Rayon" ="rayon", "Magnitude"="magnitude")
                                                               ),
                                                               
                                                            )
                                                     ),
                                                     column(width = 9,amChartsOutput("star_boxplot"))
                                                  ), 
                                                  fluidRow(
                                                     
                                                     # Effectifs des caractéristiques catégorielles :
                                                     
                                                     h2("Effectifs des caractéristiques catégorielles"),
                                                     column(width = 3,
                                                            wellPanel(
                                                               h4("Le but est ici de visualiser les effectifs de chaque variable qualitative."),
                                                               awesomeRadio(
                                                                  inputId = "choix_var_quali",
                                                                  label = "Choisissez la variable à illustrer :", 
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
              
              ############ ---- ONGLET 3 : FOCUS SUR LES CARACTERISTIQUES DES ETOILES --------------------
              
              # L'objectif est ici de mieux visualiser les données selon le type de l'étoile
              
              tabPanel("Focus sur les caractéristiques des étoiles", 
                       h1("Objectif : caractérisation des variables en fonction du type"),
                       
                       tabsetPanel(
                          
                          #### 1) Statistiques descriptives ############# 
                          
                          tabPanel("Statistiques descriptives",
                                   
                                   verticalLayout(fluid=TRUE,
                                                  fluidRow(
                                                     
                                                     # Boxplot par type d'étoile :
                                                     
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
                                                     
                                                     # Graphe bâton par type d'étoile :
                                                     
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
                          
                          ### 2) Liaison entre les variables ##########
                          
                          tabPanel("Liaison entre les variables", 
                                   verticalLayout(

                                      # Entre les variables numériques (corrélation) :
                                      
                                      h2("Entre les variables numériques : matrice des corrélations"),
                                      fluidRow(
                                         column(width = 4,verbatimTextOutput("corr_result")),
                                         column(width = 8,plotlyOutput("graph_corr", height = "400px" ))
                                      ),
                                      
                                      h3("_____________________________________________________________________________________________________________________________________________________________________"),
                                      # Entre les variables catégorielles (Test de pearson) :
                                      
                                      h2("Entre les variables catégorielles : test du khi-deux"),
                                      h3("Le test du khi-deux d'indépendance vérifie si deux variables sont susceptibles d'être liées ou pas."),
                                      verbatimTextOutput("khi2"),
                                      h3("L'hypothèse d'indépendance entre les deux variables est rejetée."),
                                      
                                   ), 
                                   br(),
                                   br(),
                          ),
                          
                          
                          ### 3) Analyse de la structure #################
                          
                          tabPanel("Analyse de la structure",
                                   
                                   h2("Analyse en composantes principales : analyse de la structure du jeu de données"),
                                   fluidRow(
                                      column(width = 3,
                                             wellPanel(
                                                colourpicker::colourInput(inputId = "colorACP", label = "Couleur :", value = "#E61C34"),
                                                colourpicker::colourInput(inputId = "colorACPsupp", label = "Couleur supplémentaire :", value = "#4E4EBA"),
                                                
                                                numericInput("dim1", "Première dimension:", 1,
                                                             min = 1, max = 4),
                                                
                                                numericInput("dim2", "Seconde dimension:", 2,
                                                             min = 1, max = 4),
                                                
                                                actionButton("goACP", "VALIDER")
                                             )
                                      ),
                                      column(width = 9,
                                             tabsetPanel(id = "vizACP",
                                                         
                                                         ## ---- 3.a) Graphe des individus --------------------
                                                         
                                                         tabPanel("Graphes des individus",
                                                                  plotlyOutput("ACP_ind"),
                                                                  plotlyOutput("ACP_ind_ellipse")
                                                         ),
                                                         
                                                         ## ---- 3.b) Graphe des variables --------------------
                                                         
                                                         tabPanel("Graphes des variables",
                                                                  plotlyOutput("ACP_var")
                                                         ),
                                                         
                                                         ## ---- 3.c) Variance expliquée --------------------
                                                         
                                                         tabPanel("Variance expliquée",
                                                                  plotlyOutput("graph_vp"),
                                                                  br(),
                                                                  br(),
                                                                  textOutput("text_vp")
                                                         ),
                                                         
                                                         ## ---- 3.d) Résumé --------------------
                                                         
                                                         tabPanel("Résumé",
                                                                  verbatimTextOutput("summaryACP")
                                                         )
                                             )
                                      )
                                   ),
                                   br(),
                                   br(),
                                   
                                   
                          ),
                       ),
              ),
              
              ############ ---- ONGLET 4  : CLASSIFICATION DES ETOILES ----------------
              
              navbarMenu("Classification des étoiles",
                         
                         ############ ---- ONGLET 4.1  : CLASSIFICATION OFFICIELLE DES ETOILES ----------------
                         
                         tabPanel("Classification officielle des étoiles",
                                  
                                  # Objectif : poser le contexte de la classification d'étoiles, présenter les différents types d'étoiles et les variables caractérisant les groupes
                                  # -> Mise en parallèle direct de la théorie versus les données 
                                  # Toutes les informations sur un seul diagramme :
                                  # - taille des points = luminosité, 
                                  # - forme des points = classes spectrales, 
                                  # - couleur = type d'étoiles, 
                                  # - abcisses = température 
                                  # - ordonnées = magnitude 
                                  
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
                                                                                        ),
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
                         
                         ############ ---- ONGLET 4.2  : MODELE PREDICTIF ----------------
                         
                         tabPanel("Modèle prédictif", 
                                  tabsetPanel(
                                     
                                     ############# 1) Prédire une nouvelle étoile ################
                                     tabPanel("Prédire une nouvelle étoile", 
                                              sidebarLayout(fluid=TRUE, 
                                                            sidebarPanel(
                                                               h3("Renseignez ci dessous des valeurs pour votre nouvelle étoile"),
                                                               textInput("temperature", "Température (entre 1939 et 40 000) :", "11550"),
                                                               textInput("magnitude", "Magnitude (entre -12 et 20) :", "-3.35"),
                                                               textInput("rayon", "Rayon (entre 0 et 2000) :", "0.16"),
                                                               textInput("titre_new_etoile", "Nom de votre étoile", "Mon étoile"),
                                                               actionButton("gopred", "VALIDER")
                                                            ), 
                                                            mainPanel(
                                                               h3("Le type prédit s'affiche ci dessous, ainsi que le diagramme HR avec votre nouvelle étoile."),
                                                               verbatimTextOutput("nouvelle_etoile"), 
                                                               plotOutput("new_pred_plot"),
                                                               downloadButton('downloadPlot', 'Télécharger le diagramme')
                                                            ),
                                              ),
                                              br(),
                                              br()
                                     ),
                                     ############# 2) Choix du modèle ################
                                     tabPanel("Choix du modèle",
                                              verticalLayout(fluid = TRUE, 
                                                             ## ---- 1.a) Recherche du meilleur modèle au sens de l'AIC et BIC --------------------
                                                             
                                                             h4("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
                                                             h3("Recherche du meilleur modèle au sens de l'AIC et BIC"),
                                                             fluidRow(
                                                                sidebarLayout(fluid = TRUE,
                                                                              sidebarPanel(plotOutput("aic_bic")),
                                                                              mainPanel(
                                                                                 h3("Modèle retenu : Type ~ Temperature + Magnitude + Rayon"),
                                                                                 verbatimTextOutput("coef_best_mod")
                                                                              )
                                                                )
                                                             ),
                                                             # Modèle multinomiale
                                                             
                                                             # Objectif : permettre dans un premier temps à l'utilisateur de choisir
                                                             # - les variables pour construire le modèle multinomiale;
                                                             # - la métrique de validation.
                                                             # -> ressort le summary, les valeurs prédites + matrices de confusion + critères 
                                                             
                                                             h3("Modèle multinomiale"),
                                                             fluidRow(
                                                                sidebarLayout(fluid = TRUE,
                                                                              sidebarPanel(checkboxGroupInput(inputId = "choix_var_mod_mult", label = "Choisissez le ou les variables à utiliser dans le modèle",
                                                                                                              choices = c("Température"="temperature", "Luminosité"="luminosite", "Rayon"="rayon", "Magnitude absolue"="magnitude"),
                                                                                                              selected = c("Température"="temperature", "Luminosité"="luminosite", "Rayon"="rayon", "Magnitude absolue"="magnitude"))),
                                                                              mainPanel(
                                                                                 tabsetPanel(
                                                                                    
                                                                                    ## ---- 1.b) Sommaire du modèle --------------------
                                                                                    
                                                                                    tabPanel("Sommaire du modèle",verbatimTextOutput("resum_mod")),
                                                                                    
                                                                                    ## ---- 1.c) Matrice de confusion --------------------
                                                                                    
                                                                                    tabPanel("Matrice de confusion",verbatimTextOutput("confusion_matrix"))
                                                                                 ),
                                                                              )
                                                                )
                                                             ),
                                                             
                                                             
                                              )
                                     ),
                                     
                                     
                                     
                                     
                                  )
                                  
                         ),
                         
                         
                         
                         
                         ############ ---- ONGLET 4.3  : ARBRE DE DECISION ----------------
                         
                         tabPanel("Arbre de décision CART",
                                  h2("Arbre de décision CART"),
                                  h3("L'objective est ici de créer un arbre de décison CART à partir des différentes caractéristiques des étoiles et de prédire avec ce modèle le type d'une nouvelle étoile."),
                                  
                                  sidebarLayout(
                                     sidebarPanel(
                                        sliderInput("taille_app", "Nombre d'individus :", 120, 240, 240,sep = 1),
                                        visNetworkOutput("arbreCART", height = '700px'),
                                     ),
                                     mainPanel( 
                                        verticalLayout(fluid=TRUE, 
                                                       h4("__________________________________________________________________"),
                                                       fluidRow(fluid=TRUE, 
                                                                column(6,
                                                                       h4("Matrice de confusion"),
                                                                       verbatimTextOutput("pred_CART"),
                                                                       verbatimTextOutput("accuracy"),
                                                                ),
                                                                column(6,
                                                                       h4("Importance des variables"),
                                                                       plotlyOutput("imp_var"),
                                                                ),
                                                                ),
                                                       h4("__________________________________________________________________"),
                                                       h4("Taux de mauvais classement en fonction de la taille de l'arbre"),
                                                       verbatimTextOutput("min"),
                                                       plotOutput("cp")
                                        )
                                        
                                     )
                                     
                                  )
                         ),
                         
                         ############ ---- ONGLET 4.4  : CLASSIFICATION ASCENDANTE HIERARCHIQUE ----------------
                         
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
                                           
                                           ############# 1) Inertie intra-groupe ################
                                           
                                           tabPanel("Inertie intra-groupe", plotlyOutput("fct_perte_coude")),
                                           
                                           ############# 2) Statistiques de Gap ################
                                           
                                           tabPanel("Statistiques de GAP", plotlyOutput("fct_perte_gap")),
                                           
                                           ############# 3) Méthode silhouette ################
                                           
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
                                                           
                                                           ############# 4) Dendogramme ################
                                                           
                                                           tabPanel("Dendogramme", plotlyOutput("plot_dendogramme"))
                                                        )
                                                 ),
                                                 column(6,
                                                        tabsetPanel(
                                                           
                                                           ############# 5) Diagramme HR ################
                                                           
                                                           tabPanel("'Diagramme HR'", plotlyOutput("hr_diag_clusters"))
                                                        )
                                                 ),
                                                 
                                              ))
                                     ,
                                     br(),
                                     br()
                                  )
                         )
                         
              ),
              
              
              tags$footer(column(6, "L'Institut Agro Rennes/Angers", icon = icon("fa-sharp fa-solid fa-house")), 
                          column(2, actionLink("twitter_share", label = "Share", icon = icon("fa-thin fa-star"),
                                               style= "color:white;", onclick = sprintf("window.open('%s')", 
                                                                                        "https://github.com/MarionMoussay/Projet-Prog-R"))),
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







