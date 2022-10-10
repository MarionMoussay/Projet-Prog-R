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
# ONGLET 4.1  : MODELE PREDICTIF
#     1) Choix du modèle
#           1.a) Sommaire du modèle
#           1.b) Matrice de confusion
#           1.c) Recherche du meilleur modèle au sens de l'AIC et BIC
#     2) Prédire une nouvelle étoile (l'objectif étant de prédire un type d'étoile pour des nouvelles entrées de variable)
# ONGLET 4.2  : CLASSIFICATION OFFICIELLE DES ETOILES
# ONGLET 4.3  : ARBRE DE DECISION
# ONGLET 4.4  : CLASSIFICATION ASCENDANTE HIERARCHIQUE (comparaison des groupes de sortie)
#     1) Inertie intra-groupe
#     2) Statistiques de Gap
#     3) Méthode silhouette
#     4) Dendogramme
#     5) Diagramme HR





shinyServer(function(input, output, session) {
    
    ############ ---- ONGLET 1  : CONTEXTE ----------------
    
    # /
    
    ############ ---- ONGLET 2 : JEU DE DONNEES --------------------
    
    #### 1) Télécharger les données #####################
    
    # Table des données :
    
    output$table <- renderDT({
        datatable(stars.V2, class = 'cell-border stripe',
                  options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
                  rownames = FALSE)})
    
    # Bouton télécharger :
    
    output$downloadCsv2 <- downloadHandler(
        filename = function() {
            paste("stars", ".csv", sep="")
        },
        content = function(file) {
            write.csv(stars.V2,file)
        }
    )
    
    #### 2) Résumés ###############################
    
    # Résumé du tableau (str) :
    
    output$str <- renderDataTable({
        stars.V2 |>
            skimr::skim() |>
            gt::gt() %>% as.data.frame() %>% select(1,2,3,15) %>% dplyr::rename(Type = skim_type, 
                                                                                Variable = skim_variable, 
                                                                                Na = n_missing, 
                                                                                Distribution = numeric.hist)
    })
    
    # Résumé statistique (summary) :
    
    output$summary <- renderDataTable({
        stars.V2 |>
            skimr::skim() |>
            gt::gt() %>% as.data.frame() %>% select(2,7,8,9,11,13) %>% dplyr::rename(Variable = skim_variable,
                                                                                     Effectifs = factor.top_counts,
                                                                                     Moyenne = numeric.mean,
                                                                                     SD = numeric.sd,
                                                                                     Q1 = numeric.p25,
                                                                                     Q3 = numeric.p75)
    })
    
    #### 3) Distribution des variables ###############################
    
    # Distribution des caractéristiques numériques :
    
    output$star_boxplot <- renderAmCharts({
        amBoxplot(stars.V2[,get(input$choix_var_boxplot)], 
                  ylab=input$choix_var_boxplot, 
                  main=paste0("Distribution de la variable ", input$choix_var_boxplot),
                  las=2, xlab="", col=terrain.colors(9))
        
    })
    
    # Effectifs des caractéristiques catégorielles :
    
    output$histo_quali<-renderPlotly({
        if (input$choix_var_quali == "spectre") { 
            legend <- "Classes spectrales"
        } else { 
            legend <- "Couleurs"
        }
        
        ggplot(stars.V2, aes(y = get(input$choix_var_quali),x = (..count..)/sum(..count..)*100, fill=get(input$choix_var_quali))) +
            geom_bar() +
            ylab(legend) + 
            labs(title="Effectifs par modalité (%)")+
            theme_minimal() +
            theme(axis.line = element_line(colour = "black"), 
                  axis.title.x = element_blank())+
            scale_fill_manual(legend, values = terrain.colors(9))
    })
    
    
    
    
    ############ ---- ONGLET 3 : FOCUS SUR LES CARACTERISTIQUES DES ETOILES --------------------
    
    #### 1) Statistiques descriptives #############
    
    # Distribution des caractéristiques numériques selon les types d'étoiles :
    
    output$star_type_boxplot <- renderAmCharts({
        amBoxplot(as.formula(paste(input$choix_var_boxplot_type,"~type")), 
                  data=stars.V2, 
                  ylab=input$choix_var_boxplot_type, 
                  main=paste0("Distribution de la variable ", input$choix_var_boxplot_type),
                  las=2, xlab="", col=terrain.colors(6))
        
    })
    
    # Effectifs des caractéristiques catégorielles selon les types d'étoiles :
    
    output$histo_quali_type<-renderPlotly({
        if (input$choix_var_quali_type == "spectre") { 
            legend <- "Classes spectrales"
        } else { 
            legend <- "Couleurs"
        }
        
        ggplot(stars.V2, aes(y = get(input$choix_var_quali_type), x = (..count..)/sum(..count..)*100, fill = type)) +
            geom_bar() +
            ylab(legend) + 
            labs(title="Effectifs selon le type d'étoiles (%)")+
            theme_minimal() +
            theme(axis.line = element_line(colour = "black"), 
                  axis.title.x = element_blank())+
            scale_fill_manual(legend, values = terrain.colors(6))
            
    })
    
    ### 2) Liaison entre les variables ##########
    
    # Entre les variables numériques (corrélation) :
    
    mat<- reactive({
        mat <- stars.V2 %>% select(-c(type, couleur, spectre)) %>% as.matrix()
        mat
    })
    
    output$corr_result <- renderPrint({
        rcorr(mat())
    })
    
    output$graph_corr<-renderPlotly({
        mat.cor <- cor(mat())
        heatmaply_cor(mat.cor, show_dendrogram = c(FALSE, FALSE))
        
    })
    
    # Entre les variables catégorielles (Test de pearson) :
    
    output$khi2 <- renderPrint({
        couleur <- stars.V2$couleur
        spectre <- stars.V2$spectre
        chisq.test(couleur, spectre)
    })
    
    # Analyse de la variance :
    
    mod_anova <- reactive({
        mod <- lm(as.formula(paste0(input$choix_var_anova,"~couleur*spectre")), data=stars.V2)
        mod
    })
    
    output$summary_anova <- renderPrint({
        Anova(mod_anova())
    })
    
    output$shapiro <- renderDataTable({
        test <- shapiro_test(residuals(mod_anova()))
        data.frame("Statistique"= test$statistic[[1]] , "P-value" = test$p.value[[1]], row.names = "Test de shapiro")
    })
    
    output$qqplot <- renderPlotly({
        ggqqplot(residuals(mod_anova()), title = "QQ-plot")
    })
    
    output$plot_mixte_spectre <- renderAmCharts({
        amBoxplot(as.formula(paste(input$choix_var_anova,"~spectre")), 
                  data=stars.V2, 
                  ylab=input$choix_var_anova, 
                  main=paste0("Distribution en fonction du spectre de la variable ", input$choix_var_anova),
                  las=2, xlab="", col=terrain.colors(7))
    })
    
    output$plot_mixte_couleur <- renderAmCharts({
        amBoxplot(as.formula(paste(input$choix_var_anova,"~couleur")), 
                  data=stars.V2, 
                  ylab=input$choix_var_anova, 
                  main=paste0("Distribution en fonction de la couleur de la variable ", input$choix_var_anova),
                  las=2, xlab="", col=terrain.colors(10))
    })
    
    
    ### 3) Analyse de la structure #################
    
    ## ---- 3.a) Graphe des individus --------------------
    res.pca <- reactive({
        res.pca <- PCA(as.data.frame(stars.V2)[,c(1,2,3,4,7)], quali.sup = 5, graph=FALSE, axes = c(input$dim1,input$dim2))
        
    })
    
    output$ACP_ind<-renderPlotly({
        input$goACP 
        isolate({
            g <- fviz_pca_ind(res.pca(), repel = TRUE,label="none", axes = c(input$dim1,input$dim2),col.ind=input$colorACP) %>% 
                fviz_add(res.pca()$quali.sup$coord, color = input$colorACPsupp, axes = c(input$dim1,input$dim2))
            ggplotly(g)
        })
    })
    
    output$ACP_ind_ellipse<-renderPlotly({
        input$goACP
        isolate({
            g <- fviz_pca_ind(res.pca(),
                              geom.ind = "point", 
                              col.ind = stars.V2$type, 
                              palette = terrain.colors(9),
                              addEllipses = TRUE, 
                              legend.title = "Star type", axes = c(input$dim1,input$dim2)) %>% 
                fviz_add(res.pca()$quali.sup$coord, color = "black", axes = c(input$dim1,input$dim2))
            ggplotly(g)
        })
        
    })
    
    ## ---- 3.b) Graphe des variables --------------------
    res.pca.quali <- reactive({
        PCA.s.quali <- PCA(stars.V2, quali.sup=5:7, axes = c(input$dim1,input$dim2))
        PCA.s.quali
    })
    
    output$ACP_var<-renderPlotly({
        input$goACP
        isolate({
            ggplotly(fviz_pca_var(res.pca.quali(), col.var = "cos2", axes = c(input$dim1,input$dim2),
                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                  label="all"
            ))
        })
    })
    
    ## ---- 3.c) Variance expliquée --------------------
    output$graph_vp<-renderPlotly({
        input$goACP
        isolate({
            ggplotly(fviz_eig(res.pca.quali(), addlabels = TRUE, ylim = c(0, 70),barfill=input$colorACP,barcolor=input$colorACP)+
                         xlab("Percentage of explained variances") +
                         ylab("Dimensions") + 
                         labs(title="Eigen values")+
                         scale_fill_identity()+
                         theme_bw() +
                         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                         theme(plot.title = element_text(hjust = 0.5,size=9)))
        })
    })
    
    output$text_vp <- renderText({ 
        
        input$goACP
        isolate({
            
            # on cherche à calculer le quantile de l'inertie des 2 premières dimensions si on permute le jeux de données
            permuteLigne <- function(v) {return(v[sample(1:length(v),replace=FALSE)])}
            
            test <- stars[,1:4]
            test
            
            nind <- nrow(test)
            nvar <- ncol(test)
            print(nind)
            print(nvar)
            nbsimul <- 1000
            iner <- NULL
            
            for (i in 1:nbsimul){
                mat <- apply(test,2,permuteLigne)
                iner <- c(iner,PCA(mat,graph=F)$eig[2,3])
            }
            
            # calcul de l'inertie du quantile (=57%)
            a <- quantile(iner,0.95)
            
            # % l'inertie du jeux de donnees (=84%) est plus grand que le quantile 95%
            b <- res.pca.quali()$eig[2,3]
            
            print(paste0("Le quantile de l'inertie avec des lignes permutées est de ",round(a), " et l'inertie de nos 2 premières dimensions est ",round(b), "."))
        })
    })
    
    ## ---- 3.d) Résumé --------------------
    
    output$summaryACP <- renderPrint({
        input$goACP
        isolate({
            PCA.s.quali <- PCA(stars.V2, quali.sup=5:7, axes = c(input$dim1,input$dim2))
            summary(PCA.s.quali, ncp=max(input$dim1,input$dim2))
        })
    })
    
    
    
    
    
    
    ############ ---- ONGLET 4  : CLASSIFICATION DES ETOILES ----------------
    
    ############ ---- ONGLET 4.1  : MODELE PREDICTIF ----------------
    
    ############# 1) Choix du modèle ################
    
    ## ---- 1.a) Sommaire du modèle --------------------
    
    mod <- reactive({
        data <- stars.V2 
        
        if (length(input$choix_var_mod_mult)== 0){
            mod <- multinom(type~1, data=data)
        } else {
            var <- c(input$choix_var_mod_mult[1])
            for (i in 1:(length(input$choix_var_mod_mult))){
                var <- paste0(var, paste0("+", input$choix_var_mod_mult[i]))
            }
            formul <- paste0("type~1+",var)
            mod <- multinom(as.formula(formul), data=data)
        }
    })
    
    output$resum_mod <- renderPrint({
        summary(mod())$coeff
    })
    
    ## ---- 1.b) Matrice de confusion --------------------
    
    pred.mod.mult <- reactive({
        data <- stars.V2 %>% select(1:5)
        predict(mod(), newdata = data)
    })
    
    output$pred <- renderPrint({
        data <- stars.V2 %>% select(1:5)
        print(table(pred.mod.mult(), data$type))
        paste0("Accuracy : ", mean(pred.mod.mult()== data$type))
    })
    
    ## ---- 1.c) Recherche du meilleur modèle au sens de l'AIC et BIC --------------------
    
    output$aic_bic <- renderPlot({
        data <- stars.V2 %>% select(luminosite, temperature, magnitude, rayon, type)
        select <- summary(regsubsets(type~.,data=as.data.frame(data),nvmax=4))
        
        data <- stars.V2 %>% select(luminosite, temperature, magnitude, rayon, type)
        bic <- select$bic
        aic <- bic - (log(nrow(data))-2)*(1:4)
        plot(1:4,bic,pch=16,bty="l",type="b",xlab="Nombre de variables explicatives",ylab="Critères",ylim=range(c(aic,bic)),
             col="darkgray",main="Sélection exhaustive du modèle",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        lines(1:4,aic,type="b",pch=17,lwd=2,col="coral1")
        legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("darkgray","coral1"),bty="n",cex=1.25,legend=c("BIC","AIC"))
        
    })
    
    output$coef_best_mod <- renderPrint({
        best.mod <- multinom(type~temperature+magnitude+rayon, data = stars.V2)
        summary(best.mod)$coeff
    })
    
    ############# 2) Prédire une nouvelle étoile ################
    
    new_star <- reactive({
        best.mod <- multinom(type~temperature+magnitude+rayon, data = stars.V2)
        pred <- predict(best.mod, newdata=data.frame('temperature' = as.numeric(input$temperature),
                                                     'magnitude' = as.numeric(input$magnitude),
                                                     'rayon' = as.numeric(input$rayon)))
        pred
    })
    
    # ERREUR ICI :
    output$nouvelle_etoile <- reactive({
        input$gopred
        isolate({
            print(paste0("Prediction de votre nouvelle étoile : ", new_star()))
        })
    })
    
    output$new_pred_plot <- renderPlot({
        input$gopred
        isolate({
            ggplot(data = stars.V2) + 
                geom_point(aes(x = temperature, y = magnitude, color = type, shape = spectre, size = luminosite)) +
                annotate("point", x=  as.numeric(input$temperature), y=as.numeric(input$magnitude), size=5, color='purple')+
                annotate(geom="text", x=as.numeric(input$temperature)+10, y=as.numeric(input$magnitude)+2, label=input$titre_new_etoile,color="purple")+
                scale_y_reverse(name="Magnitude absolue (Mv)") +
                scale_x_reverse(name = "Température ", limits=c(34000,3000)) + 
                labs(title="Diagramme Hertzsprung-Russell")+
                theme_bw() +
                theme(panel.grid.major = element_blank(), 
                      axis.line = element_line(colour = "black"), 
                      legend.text= element_text(size=15), 
                      legend.title = element_text(size=15), 
                      plot.title = element_text(size=15, face="bold.italic"))
        })
        
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$dataset, '.png', sep='') },
        content = function(file) {
            ggsave(file,plotInput())
        }
    )
    
    ############ ---- ONGLET 4.2  : CLASSIFICATION OFFICIELLE DES ETOILES ----------------
    
    output$diagramme_HR<-renderPlot({
        data <- stars.V2 %>% filter(type %in% input$choix_var_hrdiag)
        ggplot(data = data) + 
            geom_point(aes(x = temperature, y = magnitude, color = type, shape = spectre, size = luminosite)) +
            scale_y_reverse(name="Magnitude absolue (Mv)") +
            scale_x_reverse(name = "Température ", limits=c(34000,3000)) + 
            labs(title="Diagramme Hertzsprung-Russell")+
            theme_bw() +
            theme(panel.grid.major = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  legend.text= element_text(size=15), 
                  legend.title = element_text(size=15), 
                  plot.title = element_text(size=15, face="bold.italic"))
    })
    
    ############ ---- ONGLET 4.3  : ARBRE DE DECISION ----------------
    
    CART_index <- reactive({
        index <- sample(nrow(stars.V2), input$nb_ech_app)
    })
    CART_train <- reactive({
        data.train <- stars.V2[CART_index(), ] #Echantillon d’apprentissage
        data.train
    })
    
    CART_test <- reactive({
        data.test <- stars.V2[-CART_index(), ] #Echantillon de test
        data.test
    })
    
    mod_CART <- reactive({
        res <- rpart(type~., data=CART_train(), method='class')
        res
    })
 
    output$arbreCART <- renderVisNetwork({
        visTree(mod_CART(), main = "Arbre de décision CART", legend=FALSE, width = "100%")
    })
    
    output$pred_CART <- renderPrint({
        tree_opt <- prune(mod_CART(),cp=mod_CART()$cptable[which.min(mod_CART()$cptable[,4]),1])
        pred<-predict(tree_opt,newdata=CART_test(), type="class")
        mc <- confusionMatrix(CART_test()$type,pred)
        mc$table
    })
    
    output$accuracy <- renderPrint({
        tree_opt <- prune(mod_CART(),cp=mod_CART()$cptable[which.min(mod_CART()$cptable[,4]),1])
        pred<-predict(tree_opt,newdata=CART_test(), type="class")
        mc <- confusionMatrix(CART_test()$type,pred)
        
        print(paste0("Accuracy : ", mc$overall[[1]]))
    })
    
    output$cp <- renderPlot({
        plotcp(mod_CART())
    })
    
    ############ ---- ONGLET 4.4  : CLASSIFICATION ASCENDANTE HIERARCHIQUE ----------------
    
    output$choix_ultrametric <- renderUI({
        awesomeRadio(
            inputId = "choix_ultrametric",
            label = "Choisissez la distance ultramétrique souhaitée :", 
            choices = list("Distance de Ward"="ward.D", "Saut minimal" ="single", "Saut maximal" ="complete")
        )
    })
    
    matrice <- reactive({
        matrice <- stars %>% select(-c(5,6,7))
        matrice
        
    })
    
    cah <- reactive({
        d <- dist(matrice())
        cah <- hclust(d, method=input$choix_ultrametric)
        cah
    })
    
    ############# 1) Inertie intra-groupe ################
    
    output$fct_perte_coude <- renderPlotly({
        data <- data.frame("inertie" = rev(cah()$height)[1:20], "clusters" = 1:20)
        graph <- ggplot(data, aes(y= inertie, x=clusters)) + geom_line(col="#75b8d1")+geom_point(col="#75b8d1") +labs(title="Fonction de perte")+ylab("Inertie intra-groupe")+ xlab("Nombre de clusters") + theme_minimal()
        ggplotly(graph)
    })
    
    ############# 2) Statistiques de Gap ################
    
    output$fct_perte_gap <- renderPlotly({
        fviz_nbclust(matrice(),kmeans, method="gap_stat", k.max = 10) + labs(subtitle = "Gap Statistic method")
    })
    
    ############# 3) Méthode silhouette ################
    
    output$fct_perte_silhouette <- renderPlotly({
        fviz_nbclust(matrice(), kmeans, method="silhouette", k.max = 10) + labs(subtitle = "Silhouette method")
    })
    
    ############# 4) Dendogramme ################
    
    output$plot_dendogramme <- renderPlotly({
        ggplotly(fviz_dend(cah(), k = input$nb_clusters, show_labels = FALSE, rect = TRUE)+theme(legend.position = "none"))
    })
    
    ############# 5) Diagramme HR ################
    
    output$hr_diag_clusters <- renderPlotly({
        km <- kmeans(matrice(), centers = input$nb_clusters)
        df_clust <- matrice() %>% bind_cols(cluster = as.factor(km$cluster))
        graph <- ggplot(data = df_clust) +
            geom_point(aes(x = Temperature.K, y = Absolute_Magnitude.Mv, color =cluster)) +
            scale_y_reverse() +
            scale_x_reverse() +
            xlab("Temperature (K)") +
            ylab("Magnitude absolue (Mv)") +
            labs(title="Clusters CAH")+
            scale_fill_identity()+
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(plot.title = element_text(hjust = 0.5,size=9))
        ggplotly(graph)
    })
    
})