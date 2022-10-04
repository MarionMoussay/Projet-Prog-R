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


shinyServer(function(input, output, session) {
    
    ############ ---- ONGLET 1  : CONTEXTE ----------------
    
    
    
    
    ############ ---- ONGLET 2 : FOCUS SUR LES DONNEES --------------------
    
    #### Jeu de données brut #####################
    
    output$table <- renderDT({
        datatable(stars.V2, class = 'cell-border stripe',
                  options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
                  rownames = FALSE)})
    
    output$downloadCsv2 <- downloadHandler(
        filename = function() {
            paste("stars", ".csv", sep="")
        },
        content = function(file) {
            write.csv(stars.V2,file)
        }
    )
    
    #### Boxplot ###############################
    
    # Variables quantitatives
    
    output$star_boxplot <- renderAmCharts({
        amBoxplot(stars.V2[,get(input$choix_var_boxplot)], 
                  ylab=input$choix_var_boxplot, 
                  main=paste0("Distribution de la variable ", input$choix_var_boxplot),
                  las=2, xlab="", col=terrain.colors(9))
        
    })
    
    # Variables qualitatives 
    
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
    
    #### Résumés ###############################
    
    # str
    output$str <- renderDataTable({
        stars.V2 |>
            skimr::skim() |>
            gt::gt() %>% as.data.frame() %>% select(1,2,3,15) %>% dplyr::rename(Type = skim_type, 
                                                                         Variable = skim_variable, 
                                                                         Na = n_missing, 
                                                                         Distribution = numeric.hist)
    })
    
    # summary
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
    
    
    ############ ---- ONGLET 3 : VARIABLES ~ TYPE --------------------
    
    #### Statistiques descriptives #############
    
    # Distribution variables quantitatives 
    
    output$star_type_boxplot <- renderAmCharts({
        amBoxplot(as.formula(paste(input$choix_var_boxplot_type,"~type")), 
                  data=stars.V2, 
                  ylab=input$choix_var_boxplot_type, 
                  main=paste0("Distribution de la variable ", input$choix_var_boxplot_type),
                  las=2, xlab="", col=terrain.colors(6))
        
    })
    
    # Effectifs variables qualtitatives 
    
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
    
    ### ANALYSE DE STRUCTURE #################
    
    
    # Corrélations 
    
    mat<- reactive({
        mat <- stars.V2 %>% select(-type)
        levels(mat$couleur) <- 1:9
        levels(mat$spectre) <- 1:7
        mat <- mat %>% mutate(couleur = as.numeric(couleur), 
                              spectre = as.numeric(spectre)) %>% as.matrix()
        mat
    })
    
    output$corr_result <- renderPrint({
        rcorr(mat())
    })
    
    output$graph_corr<-renderPlotly({
        mat <- stars.V2 %>% select(-type)
        levels(mat$couleur) <- 1:9
        levels(mat$spectre) <- 1:7
        mat <- mat %>% mutate(couleur = as.numeric(couleur), 
                              spectre = as.numeric(spectre)) %>% as.matrix()
        mat.cor <- cor(mat())
        heatmaply_cor(mat.cor, show_dendrogram = c(FALSE, FALSE))
        
    })
    
    # ACP 
    ## ---- acp-summary --------------------
    output$summaryACP <- renderPrint({
        input$goACP
        isolate({
            
            PCA.s.quali <- PCA(stars.V2, quali.sup=5:7, axes = c(input$dim1,input$dim2))
            summary(PCA.s.quali, ncp=max(input$dim1,input$dim2))
        })
    })
    
    ## ---- acp-individus --------------------
    res.pca <- reactive({
        res.pca <- PCA(as.data.frame(stars.V2)[,c(1,2,3,4,7)], quali.sup = 5, graph=FALSE, axes = c(input$dim1,input$dim2))
        
    })
    
    output$ACP_ind<-renderPlotly({
        input$goACP 
        isolate({
            print(input$dim1)
            print(input$dim2)
            
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
    
    ## ---- acp-variables --------------------
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
    
    ## ---- acp-valeurs-propres --------------------
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
            permuteLigne <- function(v) {return(v[sample(1:length(v),replace=FALSE)])}
            #Xnew <- apply(X,2,permuteLigne)
            
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
            
            # calcul du quantile
            # 56.89314%
            a <- quantile(iner,0.95)
            
            # % d'inertie du jeux de donnees n'est pas plus grand que le quantile 95%
            # 83.85818%
            b <- res.pca.quali()$eig[2,3]
            
            print(paste0("The quantile from lines permuted is ",a, "and the inertie of the 2 dimensions is ",b))
        })
    })
    
    ############ ---- ONGLET 4  : CLASSIFIEUR, TYPE~VARIABLES ----------------
    
    ##### Diagramme HR #######
    
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
    
    
    ############# Modèle prédictif ################
    
    mod <- reactive({
        data <- stars.V2 %>% select(1:5)
        
        if (length(input$choix_var_mod_mult)== 0){
            mod <- multinom(type~1, data=data)
        } else {
            var <- c(input$choix_var_mod_mult[1])
            for (i in 1:(length(input$choix_var_mod_mult))){
                var <- paste0(var, paste0("+", input$choix_var_mod_mult[i]))
            }
            formul <- paste0("type~1+",var )
            mod <- multinom(as.formula(formul), data=data)
        }
    })
    
    output$resum_mod <- renderPrint({
        summary(mod())
    })
    
    output$pred <- renderPrint({
        data <- stars.V2 %>% select(1:5)
        table(predict(mod(), newdata = data), data$type)
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
            bestmod <- multinom(formula(select), data=train)   
            cvpredictions[segments[[k]]] = predict(bestmod,newdata=test)
        }
        
        cvpredictions
    })
    
    output$resum_loocv <- renderPrint({
        summary(pred_loocv())
    })
    
    ############# Arbre de décision ############
    
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
    
    ############# Classification Ascendante Hiérarchique ############
    
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
    output$fct_perte_coude <- renderPlotly({
        data <- data.frame("inertie" = rev(cah()$height)[1:20], "clusters" = 1:20)
        graph <- ggplot(data, aes(y= inertie, x=clusters)) + geom_line(col="#75b8d1")+geom_point(col="#75b8d1") +labs(title="Fonction de perte")+ylab("Inertie intra-groupe")+ xlab("Nombre de clusters") + theme_minimal()
        ggplotly(graph)
    })
    
    output$fct_perte_gap <- renderPlotly({
        fviz_nbclust(matrice(),kmeans, method="gap_stat", k.max = 10) + labs(subtitle = "Gap Statistic method")
    })
    
    output$fct_perte_silhouette <- renderPlotly({
        fviz_nbclust(matrice(), kmeans, method="silhouette", k.max = 10) + labs(subtitle = "Silhouette method")
    })
    
    output$plot_dendogramme <- renderPlotly({
        ggplotly(fviz_dend(cah(), k = input$nb_clusters, show_labels = FALSE, rect = TRUE)+theme(legend.position = "none"))
    })
    
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
    
    ############ ---- ONGLET 4 : A PROPOS DE NOUS ----------------
    
    
})