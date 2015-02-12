library(shiny)

shinyServer(function(input, output) {
  
  dataTable <- reactive({
    # Update only when update button is clicked 
    input$goButton
    
    # read inputs
    N         <- input$N # number of personas
    encounter <- input$encounter # number of encounters
    n1        <- vector(mode = 'numeric', length = N/2) # vector containing the first half of the population
    n2        <- vector(mode = 'numeric', length = N/2) # vector containing the second half of the population
    
    simList <- list() # list to hold simulation data tables for different encounters
    n1PostExp  <- vector(mode = 'numeric', length = N/2) # vector to store post-exposure health status of first half of population
    n2PostExp <- vector(mode = 'numeric', length = N/2) # vector to store post-exposure health status of second half of population
    
    # n1PostExps of exposure levels
    exp.level.minor    <- input$exp.level.minor
    exp.level.moderate <- input$exp.level.moderate
    exp.level.high     <- input$exp.level.high
    
    # percentage of population having selected encounters
    # if only one encounter is selected then the whole population will experience one encounter 
    if(encounter == 1){encounter.occur <- rep(0, N/2)}
    else{
      p <- input$encounter_ratio/100
      encounter.occur <- c(rep(0, p*N/2), rep(1, (1-p)*N/2))
    }
    # obtain distribution of exposure levels
    exp.levels <- expLevels(exp.level.minor, exp.level.moderate, exp.level.high, N/2)
    
    # read, and adjust if prompted, the transmission probability matrix dataframe
    if(input$adjustMatrix){
      # subtract the new desired offset
      transProbMatrix <- transProbMatrix-input$adjustMatrixn1PostExp
      # Probability has be greater than zero
      transProbMatrix[transProbMatrix < 0] <- 0
    }
    
    # simulate the population
    population <- c(rep(1:4, N*0.2) ,rep(5:6, N*0.1))
    # randomize the population  
    population <- sample(population)
    # split it in two halves
    n1 <- population[1:(length(population)/2)]
    n2 <- population[(length(population)/2+1):length(population)]
    
    # after splitting the population in two halves, these two halves will interact with each other throught the 
    # transmission matrix. For each encounter each pair will interact with each other only once. The two sides of 
    # the interaction pair might be affected so we need to calculate the post-exposure status for each member of the pair
    for(j in 1:encounter){
      for(i in 1:(N/2)){
        if(!encounter.occur[i]){
          # what is the new health status of the first persona in this pair
          n1PostExp[i]  <- simDiseaseTrans(recipient = n1[i], exposer =  n2[i], exposure = exp.levels[i], probMatrix = transProbMatrix)
          # what is the new health status of the second persona in this pair
          n2PostExp[i] <- simDiseaseTrans(recipient = n2[i], exposer =  n1[i], exposure = exp.levels[i], probMatrix = transProbMatrix)
        }
        else{
          n1PostExp[i]  <- n1[i]
          n2PostExp[i] <- n2[i] 
        }
      }
      # for each encounter store the data table in the list
      simList[[j]] <- data.table("recipient" = interchange2V(n1, n2), "exposer" = interchange2V(n2, n1), 
                                 "exposure" = interchange2V(exp.levels, exp.levels), "recipient.new" = interchange2V(n1PostExp, n2PostExp))
      
      # update the population with the new health status
      population <- sample(c(n1PostExp, n2PostExp))
      # split it in two halves
      n1 <- population[1:(length(population)/2)]
      n2 <- population[(length(population)/2+1):length(population)]
    }
    return(simList)
  })
  
  # now make some plots
  output$distsPlot <- renderPlot({  
    # Update only when update button is clicked
    input$goButton
    simDT <- isolate(dataTable()[[length(dataTable())]])
    simDT1 <- isolate(dataTable()[[1]])
    p1 <- ggplot(simDT1, aes(x = as.factor(recipient))) + geom_bar(fill = "#B5DAFF") 
    p1 <- p1 + xlab("Health Status of Population") + ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n")
    p1 <- p1 + theme_bw()
    p1 <- p1 + commonTheme
    
    p2 <- ggplot(simDT, aes(x = as.factor(exposer))) + geom_bar(fill = "#ee3344") 
    p2 <- p2 + xlab("Health Status of Exposers") + ylab("Frequency") + ggtitle("Distribution of Health Status of Exposers\n")
    p2 <- p2 + theme_bw()
    p2 <- p2 + commonTheme
    
    # recode levels of exposure
    simDT$exposure.new <- as.factor(simDT$exposure)
    levels(simDT$exposure.new) <- c("Minor", "Moderate", "High")
    
    p3 <- ggplot(simDT, aes(x = exposure.new)) + geom_bar(fill = "#66cccc") 
    p3 <- p3 + xlab("Exposure Levels") + ylab("Frequency") + ggtitle("Distribution of Exposure Levels\n")
    p3 <- p3 + theme_bw()
    p3 <- p3 + commonTheme
    
    p4 <- ggplot(simDT, aes(x = as.factor(recipient.new))) + geom_bar(fill = "#0000FF") 
    p4 <- p4 + xlab("Health Status of Population Post Exposure") + 
      ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n Post Exposure")
    p4 <- p4 + theme_bw()
    p4 <- p4 + commonTheme
    
    #     multiplot(p1, p3, p2, p4, cols = 2)
    grid.arrange(p1, arrangeGrob(p3, p4, ncol = 2), ncol=1)  
  })
  
  output$postExpPlot <- renderPlot({ 
    # Update only when update button is clicked
    input$goButton
    simDT <- isolate(dataTable()[[length(dataTable())]])
    
    p5 <- ggplot(simDT, aes(x = as.factor(recipient.new), fill = as.factor(exposure))) + geom_bar(position="dodge") 
    p5 <- p5 + facet_grid(recipient ~ exposer)
    p5 <- p5 + xlab("\nPost-Exposure Health Status of Recipients") + ylab("Frequency") 
    p5 <- p5 + ggtitle("Distribution of Post-Exposure Health Status of Recipients")
    p5 <- p5 + theme_bw() + commonTheme 
    p5 <- p5 + theme(legend.position="top", 
                     legend.title = element_text(colour="black", size=16),
                     legend.text = element_text(colour="black", size = 16),
                     strip.text.x = element_text(size=14, angle=0, face = "bold"),
                     strip.text.y = element_text(size=14, face="bold"),
                     strip.background = element_rect(colour="black", fill="#CCCCFF")) 
    p5 <- p5 + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                                 name="Exposure Levels",
                                 breaks=c("1", "2", "3"),
                                 labels=c(" Minor ", " Moderate ", " High"))
    print(p5)
    
  })
  
  output$dataTable <- renderDataTable({
    # Update only when update button is clicked
    input$goButton
    simDT <- isolate(dataTable()[[length(dataTable())]])
    
    simDT$exposure <- as.factor(simDT$exposure)
    levels(simDT$exposure) <- c("Minor", "Moderate", "High")
    setnames(simDT, names(simDT), 
             c("Health Status of Recipient", "Health Status of Exposer", "Exposure Level", 
               "Health Status of Recipient Post Exposure"))
  })
  
  output$transMatrix <- renderTable({
    # Update only when update button is clicked
    input$goButton
    # read, and adjust if prompted, the transmission probability matrix dataframe
    if(isolate(input$adjustMatrix)){
      transProbMatrix <- transProbMatrix-input$adjustMatrixValue
      transProbMatrix[transProbMatrix < 0] <- 0
    }
    transProbMatrix
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('Disease_Transmission_Simulations_',input$N, '_simPersonas_',
                                  format(Sys.time(), "%m-%d-%Y_%H-%M-%S"),'.csv', sep='') },
    content = function(file) {
      write.csv(df, file)
    })
  
})