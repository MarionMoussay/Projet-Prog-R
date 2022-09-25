library(shiny)
library(rAmCharts)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(factoextra)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$table <- renderDataTable({
        stars <- read.table("../data/stars_by_Marie.csv", sep=",", header=TRUE)
        stars
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
