shinyServer(function(input, output, session) {
    
    ############ ---- ONGLET 1  : CONTEXTE ----------------
    
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
    
    
    ############ ---- ONGLET 2 : FOCUS SUR LES DONNEES --------------------
    
    #### Jeu de données brut #####################
    
    output$table <- renderDT({
        datatable(stars, class = 'cell-border stripe',
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
    
    
    #### Distribution variables quantitatives ##
    
    output$star_type_boxplot <- renderAmCharts({
        amBoxplot(as.formula(paste(input$choix_var_boxplot,"~type")), 
                  data=stars.V2, 
                  ylab=input$choix_var_boxplot, 
                  main=paste0("Distribution de la variable ", input$choix_var_boxplot),
                  las=2, xlab="", col=terrain.colors(6))
        
    })
    
    #### Effectifs variables qualtitatives ##
    
    output$histo_quali<-renderPlotly({
        if (input$choix_var_quali == "spectre") { 
            # i <- 7
            legend <- "Classes spectrales"
        } else { 
            # i <- 9
            legend <- "Couleurs"
        }
        
        ggplot(stars.V2, aes(y = get(input$choix_var_quali), fill = type)) +
            geom_bar() +
            ylab(legend) + 
            labs(title="Repartition de la classe spectrale")+
            theme_minimal() +
            theme(axis.line = element_line(colour = "black"), 
                  axis.title.x = element_blank())+
            scale_fill_manual(legend, values = terrain.colors(6))
    })
    
    #### Résumés ##
    
    # str
    output$str <- renderPrint({
        str(stars)
    })
    
    # summary
    output$summary <- renderPrint({
        summary(stars)
    })
    
    
    #### Corrélations #####################################################
    
    mat.cor <- reactive({
        mat <- stars.V2 %>% select(-type)
        levels(mat$couleur) <- 1:9
        levels(mat$spectre) <- 1:7
        mat <- mat %>% mutate(couleur = as.numeric(couleur), 
                              spectre = as.numeric(spectre)) %>% as.matrix()
        rcorr(mat)
    })
    
    output$corr_result <- renderPrint({
        mat.cor()
    })
    
    output$graph_corr<-renderPlot({
        corrplot(mat.cor()$r, type="upper", order="hclust", tl.col="black", tl.srt=45)
    })
    
    ########## ACP ###############
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
        res.pca <- PCA(stars.V2[,c(1,2,3,4,7)], quali.sup = 5, graph=FALSE, axes = c(input$dim1,input$dim2))
        res.pca
    })
    
    output$ACP_ind<-renderPlotly({
        input$goACP 
        isolate({
            print(input$dim1)
            print(input$dim2)
            
            g <- fviz_pca_ind(res.pca(), repel = TRUE,label="none", axes = c(input$dim1,input$dim2),col.ind=input$colorACP)
            g <- fviz_add(p, res.pca()$quali.sup$coord, color = input$colorACPsupp, axes = c(input$dim1,input$dim2))
            ggplotly(g)
        })
    })
    
    output$ACP_ind_ellipse<-renderPlotly({
        input$goACP
        isolate({
            g <- fviz_pca_ind(res.pca(),
                              geom.ind = "point", 
                              col.ind = stars$Star_Type, 
                              palette = c("brown","red","grey","blue","orange","green"),
                              addEllipses = TRUE, 
                              legend.title = "Star type", axes = c(input$dim1,input$dim2))
            g <- fviz_add(g, res.pca()$quali.sup$coord, color = "black", axes = c(input$dim1,input$dim2))
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
            ggplotly(fviz_pca_var(PCA.s.quali(), col.var = "cos2", axes = c(input$dim1,input$dim2),
                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                  label="all"
            ))
        })
    })
    
    ## ---- acp-valeurs-propres --------------------
    
    output$graph_vp<-renderPlotly({
        input$goACP
        isolate({
            ggplotly(fviz_eig(PCA.s.quali(), addlabels = TRUE, ylim = c(0, 70),barfill=input$colorACP,barcolor=input$colorACP)+
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
            b <- PCA.s.quali()$eig[2,3]
            
            print(paste0("The quantile from lines permuted is ",a, "and the inertie of the 2 dimensions is ",b))
        })
    })
    
    ############# MODELE PREDICTIF ################
    
    mod <- reactive({
        data <- stars %>% select(1:5)
        
        if (length(input$choix_var_mod_mult)== 0){
            mod <- multinom(Star_Type~1, data=data)
        } else if (length(input$choix_var_mod_mult)== 1)  {
            mod <- multinom(Star_Type~input$choix_var_mod_mult, data=data)
        } else {
            var <- c(input$choix_var_mod_mult[1])
            for (i in 2:(length(input$choix_var_mod_mult))){
                var <- paste0(var, paste0("+", input$choix_var_mod_mult[i]))
            }
            formul <- paste0("Star_Type~",var )
            mod <- multinom(as.formula(formul), data=data)
        }
    })
    
    output$resum_mod <- renderPrint({
        summary(mod())
    })
    
    output$pred <- renderPrint({
        table(predict(mod(), newdata = data), data$Star_Type)
    })
    
    
    ############# CAH ################
    
    output$choix_ultrametric <- renderUI({
        awesomeRadio(
            inputId = "choix_ultrametric",
            label = "Choisissez la distance ultramétrique souhaitée :", 
            choices = list("Distance de Ward"="ward.D", "Saut minimal" ="single", "Saut maximal" ="complete")
        )
    })
    
    matrice <- reactive({matrice <- stars %>% select(-c(5,6,7))})
    
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
    
})