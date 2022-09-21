########### Functions ############

################### Descriptive text for each variable ######
D.text <- function(x = input$variable){
  row.label <- names(possum)
  respond <- data.frame("Observation number",
             "The site number where the possum was trapped",
             "Population, either Vic (Victoria) or other (New South Wales or Queensland)",
             "Gender, either m (male) or f (female)",
             "Age, in years",
             "Head length, in mm",
             "Skull width, in mm",
             "Total length, in cm",
             "Tail length, in cm",
             "Foot length, in cm",
             "Ear conch length, in cm",
             "Distance from medial canthus to lateral canthus of right eye, in cm",
             "Chest girth, in cm",
             "Belly girth, in cm")
  print(respond[,which(x == row.label)])
}

############### Plot for each variable ############
S.plot <- function(x = input$variable){
    graph <- possum[, (x)]
    if (is.numeric(graph)){
      hist(graph, xlab = x, col = "peachpuff", border = "white", main = NULL)
    }else{
      plot(graph, xlab = x, col = "peachpuff", border = "white", main = NULL)
    }
}

###############  ###################

