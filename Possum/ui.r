####################################
####      User interface       #####
####################################


shinyUI(
  ### User interface using package Shiny dashboard
  dashboardPage(skin = "green",
                
                ## Header content
                dashboardHeader(title = "We love possums",
                                titleWidth = 230),
                
                ## Sidebar content
                dashboardSidebar(
                  
                  sidebarMenu(
                    
                    # Name for tasks in sidebar
                    menuItem("Project presentation",
                             tabName = "tab1",
                             icon = icon("dashboard")
                    ),
                    menuItem("Dataset",
                             tabName = "tab2",
                             icon = icon("th")
                    ),
                    menuItem("Data visualisation",
                             tabName = "tab3",
                             icon = icon("stats", lib = "glyphicon"),
                             menuSubItem("Explore correlation", tabName = "tab3a", icon = icon("sort", lib= "glyphicon")),
                             menuSubItem("Decision tree", tabName = "tab3b", icon = icon("tree-conifer", lib= "glyphicon"))
                    ),
                    menuItem("Regression model",
                             tabName = "tab4",
                             icon = icon("road", lib = "glyphicon")
                    ),
                    menuItem("Sources",
                             tabName = "tab5",
                             icon = icon("r-project", lib = "font-awesome")
                    )
                  )
                ),
                
                ## Body content
                dashboardBody(
                  
                  useShinyFeedback(),
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                  
                  tabItems(
                    
                    # Content in tab1 : Project presentation
                    tabItem(tabName = "tab1",
                            h3(tags$b("Our project:")), ## We can change color also?
                            p(style="text-align: justify; font-size: 16px;","This application aims to understand the morphometric 
                           measurements of mountain opposum.
                  Our main idea is build a regression model to predict the age related to the body measurements of these animal,
                  modelled in the Regression model tab."),
                            br(),
                            img(src = "possum.jpg", align = "center",  style = "width:20%;"),
                            br("Web app developped by An Hoàng and Pierre Cottais.")
                    ),
                    
                    # Content in tab2 : Dataset
                    ## Show data table 
                    tabItem(tabName = "tab2",
                            h3(tags$b("What's our opposum data look like!")),
                            fluidRow(
                              box(width = 8,
                                  withSpinner(dataTableOutput('dataset'),
                                              color = "#00a65a", type = 6)),
                              box(width = 4,
                                  p("The possum data frame consists of nine morphometric 
                           measurements on each of 104 mountain brushtail possums, 
                           trapped at seven sites from Southern Victoria to 
                           central Queensland."),
                                  prettyRadioButtons(
                                    inputId = "variable",
                                    label = "Data summary", 
                                    choices = colnames(dt),
                                    status = "warning",
                                    fill = TRUE),
                                  tags$b(textOutput("des.text"),
                                         plotOutput("sum.plot"))
                              )
                            )
                    ),
                    
                    # Content in tab3 : visualisation 
                    ## Subtab 3a: Explore correlation
                    tabItem(tabName = "tab3a",
                            h3(tags$b("Linear correlation")),
                            fluidRow(
                              box(width = 4, 
                                  h4(tags$b("Correlation matrix")),
                                  hr(),
                                  withSpinner(plotOutput("matcorr", height = "500px"),
                                              color = "#00a65a", type = 6)
                              ),
                              box(width = 8,
                                  tabsetPanel(type = "tabs",
                                              tabPanel(h5(tags$b("Bivariate vizualisation")),
                                                       hr(),
                                                       wellPanel(
                                                         selectInput("vary",
                                                                     label = "Y variable",
                                                                     choices = colnames(dt),
                                                                     selected = "age"),
                                                         selectInput("varx",
                                                                     label = "X variable",
                                                                     choices = colnames(dt))
                                                       ),
                                                       plotlyOutput("ggp")  
                                              ),
                                              tabPanel(h5(tags$b("Multivariable visualisation")),
                                                       hr(),
                                                       wellPanel(
                                                         pickerInput(inputId = "vary_multi",
                                                                     label = "Y variable", 
                                                                     choices = list(
                                                                       factors = names(which(sapply(dt, is.factor))),
                                                                       numerics = names(which(!sapply(dt, is.factor)))
                                                                     ),
                                                                     selected = "age"),
                                                         pickerInput(inputId = "varx_multi",
                                                                     label = "X variable", 
                                                                     choices = list(
                                                                       factors = names(which(sapply(dt, is.factor))),
                                                                       numerics = names(which(!sapply(dt, is.factor)))
                                                                     ),
                                                                     selected = "hdlngth"),
                                                         pickerInput(inputId = "varz_multi",
                                                                     label = "Corlor variable", 
                                                                     choices = list(
                                                                       factors = names(which(sapply(dt, is.factor))),
                                                                       numerics = names(which(!sapply(dt, is.factor)))
                                                                     ),
                                                                     selected = "sex"),
                                                         uiOutput("msg_err")
                                                       ),
                                                       plotlyOutput("ggp_multi"))
                                              
                                  ))
                            )
                    ),
                    ## Subtab 3b: Decision tree
                    
                    tabItem(tabName = "tab3b",
                            h3(tags$b("Decision tree")),
                            fluidRow(
                              column(width = 3,
                                     br(), br(),
                                     box(width = 12,
                                         numericInput(inputId = "minsplit",
                                                      label = "Min. number of observations that must exist in a node",
                                                      value = 20),
                                         numericInput(inputId = "minbucket",
                                                      label = "Min. number of observations in any terminal node (leaf)",
                                                      value = 7),
                                         numericInput(inputId = "cp",
                                                      label = "Complexity",
                                                      value = 0.001,
                                                      step = 0.01)
                                     )
                              ),
                              column(width = 9,
                                     withSpinner(visNetworkOutput("tree", height = "600px"),
                                                 color = "#00a65a", type = 6)
                              )
                            )
                    ),
                    
                    # Regression tab
                    tabItem(tabName = "tab4",
                            h3(tags$b("Stepwise regression")),
                            fluidRow(
                              column(width = 5,
                                     h5(tags$b("Regression fitting on the full data")),
                                     hr(),
                                     box(width = 12,
                                         
                                         # variable selection
                                         checkboxGroupButtons(
                                           inputId = "var_model",
                                           label = "Select variables for the model", 
                                           choices = colnames(dt.quanti[,-1]),
                                           direction = "vertical",
                                           checkIcon = list(
                                             yes = tags$i(class = "fa fa-circle", 
                                                          style = "color: #00a65a"),
                                             no = tags$i(class = "fa fa-circle-o", 
                                                         style = "color: #00a65a"))
                                         ),
                                         
                                         # best models from regsubset display
                                         actionButton(inputId = "best_modBT",
                                                      label = "Show me the best models!",
                                                      icon = icon("trophy"))
                                     ),
                                     verbatimTextOutput("print_best_mod")
                              ),
                              column(width = 7,
                                     h5(tags$b("Regression criteria visualisation")),
                                     hr(),
                                     tabBox(width = 12,
                                            tabPanel("AIC/BIC",
                                                     withSpinner(plotOutput("aic_bic"),
                                                                 color = "#00a65a", type = 6)
                                            ),
                                            tabPanel("RSS/PRESS",
                                                     withSpinner(plotOutput("rss_press"),
                                                                 color = "#00a65a", type = 6))
                                     )
                              )
                            )
                            
                    ),
                    
                    ## Sources tab
                    
                    tabItem(tabName = "tab5",
                            h3(tags$b("Data source")),
                            p(style="text-align: justify; font-size: 16px;","This dataset is provided in", a("DAAG R package",href = "https://cran.r-project.org/web/packages/DAAG/DAAG.pdf",  target="_blank"),
                              "or by dataset collection of", a("Kaggle",href = "https://www.kaggle.com/abrambeyer/openintro-possum",  target="_blank"),"."),
                            br(),
                            h3(tags$b("Source code")),
                            actionButton(inputId = "app_codeBT",
                                         label = a("Download Shiny app folder",
                                                   href="App_Cottais_Hoang.zip", target="_blank"),
                                         icon = icon("download"))
                    )
                    
                  )
                )
  )
) #shiny
