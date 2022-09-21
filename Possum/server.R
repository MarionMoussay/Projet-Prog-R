shinyServer(function(input, output) {
    
    #Tab2 box1: 
    output$dataset <- renderDataTable(possum,
                                      options = list(
                                          lengthMenu = list(c(10, 50, -1), c('10', '50', 'all')),
                                          pageLength = 15,
                                          scrollX = TRUE))
    
    #Tab2 box2: explain text for variable and histogram/graph
    output$des.text <- renderText(
        D.text(input$variable)
    )
    
    output$sum.plot <- renderPlot(
        S.plot(input$variable)
    )
    
    #Tab3 box1:
    output$matcorr <- renderPlot({
        corrplot::corrplot(mat_corr,
                           method = "color",
                           outline = TRUE,
                           tl.srt = 45)
    })
    
    #tab3 box2:
    ## bivariate analysis
    observeEvent(c(input$vary, input$varx), {
        if(input$vary == input$varx){
            showFeedbackWarning("vary",
                                icon = icon("exclamation-triangle"),
                                text = "Please select another Y variable to plot a bivariate graph.")
            showFeedbackWarning("varx",
                                icon = icon("exclamation-triangle"),
                                text = "Please select another X variable to plot a bivariate graph.")
        }else{
            hideFeedback("vary")
            hideFeedback("varx")
        }
    })
    
    ###Draw graph
    reac_varx <- reactive(input$varx)
    
    output$ggp <- plotly::renderPlotly({
        ggp <- ggplot(dt) + aes(x = get(reac_varx()), y = get(input$vary)) +
            geom_point() + 
            labs(x = input$varx, y = input$vary) +
            theme_minimal() + ggtitle(paste("Possum", tags$em(input$vary), "by", tags$em(input$varx)))
        ggplotly(ggp)
    })
    
    ## multivariate analysis
    
    observeEvent(c(input$vary_multi, input$varx_multi, input$varz_multi), {
        if(length(unique(c(input$vary_multi, input$varx_multi, input$varz_multi)))<3){
            # showFeedbackWarning("vary_multi",
            #                     icon = icon("exclamation-triangle"),
            #                     text = "Please select three different variables.")
            output$msg_err <- renderUI(tags$span(class = "msg_error", "Please select three different variables."))
        }
    })
    
    output$ggp_multi <- plotly::renderPlotly({
        ggp <- ggplot(dt) + aes(x = get(input$varx_multi),
                                y = get(input$vary_multi),
                                color = get(input$varz_multi))+
            geom_point() + 
            labs(x = input$varx_multi, y = input$vary_multi, color = input$varz_multi) +
            theme_minimal() + ggtitle(paste("Possum", tags$em(input$vary_multi), "by", tags$em(input$varx_multi)))
        ggplotly(ggp)
    })
    
    # display decision tree
    output$tree <- renderVisNetwork({
        # parameters for unprunned tree
        control.par <- rpart.control(minsplit = input$minsplit,
                                     minbucket = input$minbucket,
                                     cp = input$cp)
        # decision tree
        mod_tree <- rpart(age ~ ., data = dt.quanti, control = control.par)
        visTree(mod_tree)
    })
    
    
    # regression
    output$rss_press <- renderPlot({
        #### Plott rss and press cross-validation for each best submodel
        plot.new()
        plot(1:9,select$rss,type="b",pch=16,xlab="Number of variables in the submodel",
             ylab="Residual sum-of-squares",main="Quality of fit of best submodels",
             ylim = c(min(select$rss),420),
             cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="#00a65a",bty="l")
        lines(1:9,error$error,type="b",col="steelblue",pch=15,lwd=2)
        legend("topleft",lwd=2,pch=c(16,15),legend=c("Internal validation (RSS)","Cross validation (PRESS)"),
               bty="n",cex=1.25,col=c("#00a65a","steelblue"))
        
        if (!is.null(input$var_model)){
            mod_user <- glm(age ~ ., data = dt.quanti[, c("age", input$var_model)])
            cvmod <- boot::cv.glm(dt.quanti[, c("age", input$var_model)], mod_user,K=10)
            press <- cvmod$delta[2]*nrow(dt.quanti)
            abline(h=press, col = "peachpuff", lwd=2)
            legend("top", lwd=2, pch=c(16,15),
                   legend=ifelse(length(input$var_model)==1,
                                 "PRESS (new model with 1 variable)",
                                 paste("PRESS (new model with", as.character(length(input$var_model)),"variables)")),
                   bty="n",cex=1.25,col="peachpuff")
        }
        
    })
    
    output$aic_bic <- renderPlot({
        # Plot AIC/BIC
        plot.new()
        plot(1:9,var_bic,pch=16,bty="l",type="b",xlab="Number of explanatory variables",
             ylab="Information criterion",ylim=range(c(400,max(var_bic))),col="steelblue",
             main="Exhaustive model selection",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        lines(1:9,var_aic,type="b",pch=17,lwd=2,col="#00a65a")
        legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("steelblue","#00a65a"),bty="n",cex=1.25,legend=c("BIC","AIC"))
        
        if (!is.null(input$var_model)){
            mod_user <- glm(age ~ ., data = dt.quanti[, c("age", input$var_model)])
            aic <- mod_user$aic
            abline(h=aic, col = "peachpuff", lwd=2)
            legend("top", lwd=2, pch=c(16,15),
                   legend=ifelse(length(input$var_model)==1,
                                 "AIC (new model with 1 variable)",
                                 paste("AIC (new model with", as.character(length(input$var_model)),"variables)")),
                   bty="n",cex=1.25,col="peachpuff")
        }
    })
    
    
    observeEvent(input$best_modBT, {
        output$print_best_mod <- renderPrint({
            select$outmat
        })
    })
    
    
    # output$app_codeDL <- downloadHandler(
    #     filename = function() {"Shinyapp_Cottais_Hoang.zip"},
    #     content = function() {
    #     }
    # )
    
})
