
fluidPage(
   # shinythemes::themeSelector(),
   # navbarPage
   navbarPage("Classification des étoiles", 
              theme = shinytheme("sandstone"),
              
              ########## ONGLET 1 ###############
              
              tabPanel("Contexte",
                       
                       ### Objectif : poser le contexte de la classification d'étoiles, présenter les différents types d'étoiles et les variables caractérisant les groupes
                       ### -> Mise en parallèle direct de la théorie versus les données 
                       ### Toutes les informations sur un seul diagramme :
                       ### - taille des points = luminosité, 
                       ### - forme des points = classes spectrales, 
                       ### - couleur = type d'étoiles, 
                       ### - abcisses = température 
                       ### - ordonnées = magnitude 
                       
                       h1("Le diagramme de Hertzsprung-Russell, la réference officielle de classification des étoiles"),
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
                              
                       )
                       
                       
              ),
              
              ##### ONGLET 2 : TELECHARGEMENT JEU DE DONNEES #####
              
              ### On expose le jeu de données dans sa globalité, possibilité de le télécharger
              
              tabPanel("Jeu de données",
                       h1("Jeu de données", style = "color : #0099ff;text-align:center"),
                       h4("Le jeu de données comportent 40 étoiles de chaque type.",style = "color : #0099ff;text-align:center"),
                       dataTableOutput("table"),
                       downloadButton("downloadCsv2", "Télécharger"),tags$br(),tags$br()
              ),
              
              ########## ONGLET 3 ###############
              
              # L'objcitf est ici de mieux visualiser les données selon le type de l'étoile
              
              navbarMenu("Focus sur les données", 
                         
                         ##### STATISTIQUES DESCRIPTIVES #####
                         
                         tabPanel("Statistiques descriptives",
                                  tabsetPanel(
                                     
                                     ## BOXPLOT PAR TYPE D'ETOILES ##
                                     
                                     tabPanel("Exploration des variables",
                                              verticalLayout(fluid=TRUE,
                                                             fluidRow(
                                                                h2("Distribution des caractéristiques numériques selon les types d'étoiles"),
                                                                column(width = 3,
                                                                       wellPanel(
                                                                          h4("Le but est ici de visualiser la distribution de chaque variable quantitive en fonction du type de l'étoile."),
                                                                          awesomeRadio(
                                                                             inputId = "choix_var_boxplot",
                                                                             label = "Choisissez la variable à illuster :", 
                                                                             choices = list("Température"="temperature", "Luminosité" ="luminosite", "Rayon" ="rayon", "Magnitude"="magnitude")
                                                                          ),
                                                                          
                                                                       )
                                                                ),
                                                                column(width = 9,amChartsOutput("star_type_boxplot"))
                                                             ), 
                                                             fluidRow(
                                                                h2("Effectifs des caractéristiques catégorielles selon les types d'étoiles"),
                                                                column(width = 3,
                                                                       wellPanel(
                                                                          h4("Le but est ici de visualiser les effectifs de chaque variable qualitative selon les types des étoiles."),
                                                                          awesomeRadio(
                                                                             inputId = "choix_var_quali",
                                                                             label = "Choisissez la variable à illuster :", 
                                                                             choices = list("Classe spectrale"="spectre", "Couleur" ="couleur")
                                                                          ),
                                                                          
                                                                       )
                                                                ),
                                                                column(width = 9, plotlyOutput("histo_quali"))
                                                                
                                                             )
                                                             
                                              ),
                                     ), 
                                     
                                     ## CORRELOGRAMME ET TEST DE CORRELATION
                                     
                                     tabPanel("Corrélations", 
                                              h2("Corrélations et liens entre les caractéristiques"),
                                              fluidRow(
                                                 column(width = 4,verbatimTextOutput("corr_result")),
                                                 column(width = 8,plotOutput("graph_corr", height = "400px" ))
                                              ),
                                     ),
                                     
                                     ## RESUMES STATISTIQUES 
                                     
                                     tabPanel("Résumés", 
                                              h4("Résumé du tableau"),
                                              verbatimTextOutput("str"),
                                              h4("Résumé statistique"),
                                              verbatimTextOutput("summary")
                                     )
                                  )
                         ),
                         ##### ACP #####
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
                                  
                         ),
              ),
              
              ########## ONGLET 5 ###############
              
             
              tabPanel("Décision du type d'étoile",
                       navbarMenu(id="onglet4",
                                  tabPanel("Arbre CART"
                                           ## Obj :  décomposer la prise de décision
                                           
                                           ),
                                  tabPanel("Modèle prédictif", 
                                           
                                           ## Obj : prédire le type
                                           tabPanel("Modèle multinomiale",
                                                    sidebarLayout(fluid = TRUE,
                                                                  sidebarPanel(checkboxGroupInput(inputId = "choix_var_mod_mult", label = "Choisissez le ou les variables à utiliser dans le modèle",
                                                                                                  choices = c("Temperature (K)"="Temperature.K", "Luminosité (L.lo)"="Luminosity.L.Lo", "Radius (R.ro)"="Radius.R.Ro", "Magnitude Absolue"="Absolute_Magnitude.Mv"))),
                                                                  mainPanel(
                                                                     tabsetPanel(tabPanel("Sommaire du modèle",verbatimTextOutput("resum_mod")),
                                                                                 tabPanel("Plot",
                                                                                          #courbe de régression
                                                                                 ),
                                                                                 tabPanel("Matrice de confusion",
                                                                                          verbatimTextOutput("pred")
                                                                                 )
                                                                     ),
                                                                  )
                                                    )),
                                           tabPanel("Ré-échantillonnage leave-one-out",
                                                    sidebarLayout(
                                                       sidebarPanel(),
                                                       mainPanel())
                                           )
                                  )
                                  
                       )
              ),
              
              
              ########## ONGLET 5 ###############
              tabPanel("Classification Ascendante Hiérarchique", 
                       ## Obj : comparer les groupes obtenus 
                       
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
              tabPanel("Autre jeu de données"),
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
              
   )
)






