library(shiny)
library(rAmCharts)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$histogramme <- renderAmCharts({
        input$button
        isolate({
            x <- faithful[, input$var]
            bins <- round(seq(min(x), max(x), length.out = input$bins + 1), 2)
            # use amHist
            amHist(x = x, control_hist = list(breaks = bins),
                   col = input$color, main = input$titre,
                   export = TRUE, zoom = TRUE)
        })
    })
    
    
    output$boxplot <- renderAmCharts({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, input$var] 
        
        # draw the histogram with the specified number of bins
        amBoxplot(x, col = input$color)
        
    })
    
    # summary
    output$summary <- renderPrint({
        summary(faithful)
    })
    
    # table
    output$table <- renderDataTable({
        stars <- read.table("../data/stars_by_Marie.csv", sep=",", header=TRUE)
        stars
    })
    
    # nombre de classe
    output$n_bins <- renderText({
        paste("Nombre de classes : ", input$bins)
    })
    
})
