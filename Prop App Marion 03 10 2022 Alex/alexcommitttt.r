#Server.r

############# Modèle prédictif ################

mod <- reactive({
  data <- stars %>% select(1:5)
  
  if (length(input$choix_var_mod_mult)== 0){
    mod <- multinom(Star_Type~1, data=data)
  } else {
    var <- c(input$choix_var_mod_mult[1])
    for (i in 1:(length(input$choix_var_mod_mult))){
      var <- paste0(var, paste0("+", input$choix_var_mod_mult[i]))
    }
    formul <- paste0("Star_Type~1+",var )
    mod <- multinom(as.formula(formul), data=data)
  }
})

output$resum_mod <- renderPrint({
  summary(mod())
})

output$pred <- renderPrint({
  table(predict(mod(), newdata = data), data$Star_Type)
})


pred_loocv <- reactive({
  data <- stars %>% select(1:5)
  n <- nrow(data)                   
  segments <- pls::cvsegments(k=240,N=n) 
  cvpredictions <- rep(0,n)   
  
  for (k in 1:240) {
    train <- data[-segments[[k]],]  
    test <- data[segments[[k]],]     
    mod <- multinom(Star_Type~.,data=train)
    select <- stepwise(mod,data=data,direction=input$choix_bf, criterion="AIC", trace=0)
    print(paste0("nombre de variables sélectionnés:", length(coef(select)-1)))
    bestmod = multinom(formula(select), data=train)   
    cvpredictions[segments[[k]]] = predict(bestmod,newdata=test)
  }
})

output$resum_loocv <- renderPrint({
  summary(bestmod())
})

resum_aic_bic <- reactive({
  data <- stars %>% select(1:5)
  select <- summary(regsubsets(Star_Type~.,data=data,nvmax=4))
  bic <- select$bic                           
  aic <- bic - (log(nrow(data))-2)*(1:4) 
  plot(1:4,
       bic,
       pch=16,
       bty="l",
       type="b",
       xlab="Number of explanatory variables",
       ylab="Information criterion",
       ylim=range(c(aic,bic)),
       col="darkgray",
       main="Exhaustive model selection",
       cex.lab=1.25,
       cex.axis=1.25,
       cex.main=1.25,
       lwd=2)
  lines(1:4,
        aic,
        type="b",
        pch=17,
        lwd=2,`
        col="coral1")
  legend("topleft",
         lwd=2,
         lty=1,
         pch=c(16,17),
         col=c("darkgray","coral1"),
         bty="n",
         cex=1.25,
         legend=c("BIC","AIC"))
  grid()
})

resum_bestmod <- reactive({
  data <- stars %>% select(1:5)
  select <- summary(regsubsets(Star_Type~.,data=data,nvmax=4))
  best_select <- select$which
})
output$resum_bestmod <- renderPrint({
  summary(bestmod())
})  
  








#Ui.r

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
                        
                        h4("LOOCV cross validation"),
                        fluidRow(
                          sidebarLayout(fluid = TRUE,
                                        sidebarPanel(checkboxGroupInput(inputId = "choix_bf", label = "Choisissez le sens de l'algorithme",
                                                                        choices = c("forward","backward","forward/backward")))),
                          mainPanel(
                            tabsetPanel(tabPanel("Sommaire du modèle loocv",verbatimTextOutput("resum_loocv")),
                                        tabPanel("Selection des meilleurs modèles",plotOutput("resum_aic_bic"),verbatimTextOutput("resum_bestmod")),
                                        )
                          )
                        ),
         )
)



Concernant le contexte:
  
L'évolution des technologies permet de receuillir des informations en grandes quantités 
Dans le cadre de l'astronomie par exemple, une multitudes de paramètres sont mesuré pour décrire l'univers qui nous 
entoure. Ici,nous nous interessons aux étoiles et cherchons à savoir si avec seulement quelques paramètres considérés important par les professionels comme ...
nous pouvons determiner avec une bonne précision la nature des étoiles.
Les étoiles sont actuellement délimités en plusieurs groupes https://www.vectorstock.com/royalty-free-vector/different-types-stars-in-dark-space-vector-28552140.
Notre jeu de données contenant 240 individus est découpé en 6 catégories d'étoiles ... equitablement réparties dans le dataset.
  



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
                        )
         )
)