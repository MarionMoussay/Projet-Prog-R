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
            choices = list("Distance de Ward"="ward.D", "Saut minimal" ="simple", "Saut maximal" ="complete")
        )
    })
    matrice <- reactive({matrice <- stars %>% select(-c(5,6,7))})
    
    cah <- reactive({
        d <-  dist(matrice())
        if (input$choix_ultrametric == "ward.D") {
            cah <- hclust(d, method = "ward.D")
        } else if (input$choix_ultrametric == "simple"){
            cah <- hclust(d, method = "simple")
        } else {
            cah <- hclust(d, method = "complete")
        }
        cah
    })
    output$fct_perte_coude <- renderPlotly({
        data <- data.frame("inertie" = rev(cah()$height)[1:20], "clusters" = 1:20)

        diff <- c()
        for (i in 2:10){
            diff <- cbind(diff, data$inertie[i-1]-data$inertie[i])
        }
        nb <-  which.min(diff)-1
        graph <- ggplot(data, aes(y= inertie, x=clusters)) + geom_line(col="#75b8d1")+geom_point(col="#75b8d1") +labs(title="Fonction de perte")+ylab("Inertie intra-groupe")+ xlab("Nombre de clusters") + theme_minimal() + geom_vline(xintercept = nb, linetype="dotted", color = "#75b8d1", size=0.5)
        ggplotly(graph)

    })
    
    output$fct_perte_gap <- renderPlotly({
        fviz_nbclust(matrice(),kmeans, method="gap_stat", k.max = 10) + labs(subtitle = "Gap Statistic method")
    })
    
    output$fct_perte_silhouette <- renderPlotly({
        fviz_nbclust(matrice(), kmeans, "silhouette", k.max = 10) + labs(subtitle = "Silhouette method")
    })
    
})
