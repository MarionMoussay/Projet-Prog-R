# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ## ---- CONTEXTE ----------------
  
  output$choix_var_hrdiag <- renderUI({
    checkboxGroupInput(inputId = "choix_var_hrdiag", label = "Choisissez le ou les type(s) à représenter", 
                       choices = c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche"), 
                       selected = c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche"))
   
  })
  
  output$diagramme_HR1<-renderPlotly({
    data <- stars
    levels(data$Star_Type) <- c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche")
    data <- data %>% filter(Star_Type %in% input$choix_var_hrdiag)
    graph <- ggplot(data = data) + 
      geom_point(aes(x = Temperature.K, y = Absolute_Magnitude.Mv, color = Star_Type)) +
      scale_y_reverse() +
      scale_x_reverse() +
      
      xlab("Température (K)") +
      ylab("Magnitude absolue (Mv)") + 
      labs(title="Diagramme HR")+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9))
    ggplotly(graph)
  })
  
  output$diagramme_HR2<-renderPlotly({
    data <- stars
    levels(data$Star_Type) <- c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche")
    data <- data %>% filter(Star_Type %in% input$choix_var_hrdiag)
    graph <- ggplot(data = data) + 
      geom_point(aes(x = Temperature.K, y = Absolute_Magnitude.Mv, color = Star_Type, shape = Spectral_Class)) +
      scale_shape_manual(values =  c(7:1)) +
      scale_y_reverse() +
      scale_x_reverse() +
      
      xlab("Temperature (K)") +
      ylab("Magnitude absolue (Mv)") + 
      labs(title="Diagramme HR")+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9), legend.title=element_text("Classe spectrale et types d'étoiles"))
    ggplotly(graph)
  })
  
  output$diagramme_HR3<-renderPlotly({
    data <- stars
    levels(data$Star_Type) <- c("Naine brune", "Hyper géante", "Séquence principale", "Naine Rouge", "Super géante", "Naine blanche")
    data <- data %>% filter(Star_Type %in% input$choix_var_hrdiag)
    graph <- ggplot(data = data) +
      geom_point(aes(x = Temperature.K, y = Luminosity.L.Lo, color = Star_Type)) +
      scale_x_reverse() +

      xlab("Temperature (K)") +
      ylab("Luminosité (L.Lo)") +
      labs(title="Diagramme HR")+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9))
    ggplotly(graph)
  })
  
  ## ---- JEUX DE DONNEES --------------------
  
  # table
  output$table <- renderDT({
    datatable(stars, class = 'cell-border stripe',
              options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
              rownames = FALSE)})
  
  output$downloadCsv2 <- downloadHandler(
    filename = function() {
      paste("stars", ".csv", sep="")
    },
    content = function(file) {
      write.csv(stars,file)
    }
  )
  
  ## ---- STATISTIQUES DESCRIPTIVES --------------------
  
  ### Vue globale ###############
  # str
  output$str <- renderPrint({
    str(stars)
  })
  
  # summary
  output$summary <- renderPrint({
    summary(stars)
  })
  
  # distribution
  output$count_type<-renderPlotly({
    count.s <- c()
    for (i in 1:length(levels(stars$Star_Type))){
      a <- as.numeric(count(stars,stars$Star_Type)[i,2])
      print(a)
      count.s <- c(count.s,a)
    }
    count.s
    
    df <- data.frame(type=levels(stars$Star_Type),number=count.s)
    
    fig <- plot_ly(df, labels = ~type, values = ~count.s, type = 'pie')
    fig <- fig %>% layout(title = 'Distribution of star type',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  output$count_class<-renderPlotly({
    ggplotly(ggplot(stars, aes(y = Spectral_Class, color = Spectral_Class)) +
               geom_bar(fill="white") + 
               xlab("Nombre") +
               ylab("Classe spectrale") + 
               labs(title="Repartition de la classe spectrale")+
               scale_fill_identity()+
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
               theme(plot.title = element_text(hjust = 0.5,size=9)))
  })
  
  output$count_color<-renderPlotly({
    ggplotly(ggplot(stars, aes(y = Star_Color, color = Star_Color)) +
               geom_bar(fill="white") + 
               xlab("Nombre") +
               ylab("Couleur de l'étoile") + 
               labs(title="Repartition de la couleur de l'étoile")+
               scale_fill_identity()+
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
               theme(plot.title = element_text(hjust = 0.5,size=9)))
  })
  
  ########## ACP ###############
  ## ---- acp-summary --------------------
  
  output$summaryACP <- renderPrint({
    input$goACP
    isolate({
      
      PCA.s.quali <- PCA(stars, quali.sup=5:7, axes = c(input$dim1,input$dim2))
      
      summary(PCA.s.quali, ncp=max(input$dim1,input$dim2))
    })
  })
  
  ## ---- acp-individus --------------------
  
  output$ACP_ind<-renderPlotly({
    input$goACP # input declenchant la reactivite
    # reste du code isole
    isolate({
      print(input$dim1)
      print(input$dim2)
      
      res.pca <- PCA(stars[,c(1,2,3,4,7)], quali.sup = 5, graph=FALSE, axes = c(input$dim1,input$dim2))
      
      p <- fviz_pca_ind(res.pca, repel = TRUE,label="none", axes = c(input$dim1,input$dim2))
      p <- fviz_add(p, res.pca$quali.sup$coord, color = input$colorACPsupp, axes = c(input$dim1,input$dim2))
      ggplotly(p)
    })
  })
  
  output$ACP_ind_ellipse<-renderPlotly({
    input$goACP
    isolate({
      res.pca <- PCA(stars[,c(1,2,3,4,7)], quali.sup = 5, graph=FALSE, axes = c(input$dim1,input$dim2))
      
      e <- fviz_pca_ind(res.pca,
                        geom.ind = "point", # Montre les points seulement (mais pas le "text")
                        col.ind = stars$Star_Type, # colorer by groups
                        palette = c("brown","red","grey","blue","orange","green"),
                        addEllipses = TRUE, # Ellipses de concentration
                        legend.title = "Star type", axes = c(input$dim1,input$dim2))
      e <- fviz_add(e, res.pca$quali.sup$coord, color = "black", axes = c(input$dim1,input$dim2))
      ggplotly(e)
    })
    
  })
  
  ## ---- acp-variables --------------------
  
  output$ACP_var<-renderPlotly({
    
    input$goACP
    isolate({
      PCA.s.quali <- PCA(stars, quali.sup=5:7, axes = c(input$dim1,input$dim2))
      
      ggplotly(fviz_pca_var(PCA.s.quali, col.var = "cos2", axes = c(input$dim1,input$dim2),
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                            label="all"
      ))
    })
  })
  
  ## ---- acp-valeurs-propres --------------------
  
  output$graph_vp<-renderPlotly({
    input$goACP
    isolate({
      PCA.s.quali <- PCA(stars, quali.sup=5:7, axes = c(input$dim1,input$dim2))
      
      ggplotly(fviz_eig(PCA.s.quali, addlabels = TRUE, ylim = c(0, 70),barfill="white",barcolor=input$colorACP)+
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
      PCA.s.quali <- PCA(stars, quali.sup=5:7, axes = c(input$dim1,input$dim2))
      
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
      b <- PCA.s.quali$eig[2,3]
      
      print(paste0("The quantile from lines permuted is ",a, "and the inertie of the 2 dimensions is ",b))
    })
  })
  
  ########## Boxplot ###############
  
  output$choix_var_graph <- renderUI({
    awesomeRadio(
      inputId = "choix_var_graph",
      label = "Choisissez la variable à illuster :", 
      choices = list("Température"="Temperature.K", "Luminosité" ="Luminosity.L.Lo", "Rayon" ="Radius.R.Ro", "Magnitude"="Absolute_Magnitude.Mv")
    )
  })
  
  output$distribution_boxplot1 <- renderAmCharts({
    amBoxplot(stars[,get(input$choix_var_graph)], ylab= input$choix_var_graph, main=paste0("Distribution de la variable ",input$choix_var_graph),las=2,col=input$color, mainColor = input$color, xlab="")

  })
  
  output$star_type_boxplot1 <- renderAmCharts({
    
    input$go
    isolate({
      
      if (input$choix_var_graph == "Temperature.K") {
        titre <- paste("Distribution de la variable 'Température'")
        amBoxplot(Temperature.K~Star_Type, data = stars, labelRotation = -45, col = input$color) %>%
          amOptions(main = titre, mainColor = input$color, mainSize = 14)
      } else if (input$choix_var_graph == "Luminosity.L.Lo"){
        titre <- paste("Distribution de la variable 'Luminosité'")
        amBoxplot(Luminosity.L.Lo~Star_Type, data = stars, labelRotation = -45, col = input$color) %>%
          amOptions(main = titre, mainColor = input$color, mainSize = 14)
      } else if (input$choix_var_graph == "Radius.R.Ro"){
        titre <- paste("Distribution de la variable 'Angle relatif")
        amBoxplot(Radius.R.Ro~Star_Type, data = stars, labelRotation = -45, col = input$color) %>%
          amOptions(main = titre, mainColor = input$color, mainSize = 14)
      } else {
        titre <- paste("Distribution de la variable 'Magnitude relative'")
        amBoxplot(Absolute_Magnitude.Mv~Star_Type, data = stars, labelRotation = -45, col = input$color) %>%
          amOptions(main = titre, mainColor = input$color, mainSize = 14)
      }
    })
  })
  
  
  
  
  
  ############# MODELE PREDICTIF ################
  
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
