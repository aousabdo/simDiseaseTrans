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
    
    # obtain distribution of exposure levels
    exp.levels <- expLevels(exp.level.minor, exp.level.moderate, exp.level.high, N/2)
    
    # percentage of population having selected encounters
    # if only one encounter is selected then the whole population will experience one encounter 
    if(encounter == 1){encounter.occur <- rep(0, N/2)}
    else{
      p <- input$encounter_ratio/100
      encounter.occur <- c(rep(0, p*N/2), rep(1, (1-p)*N/2))
    }
    
    # read, and adjust if prompted, the transmission probability matrix dataframe
    if(input$adjustMatrix){
      # subtract the new desired offset
      transProbMatrix <- transProbMatrix-input$adjustMatrixValue
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
          n2PostExp[i]  <- n2[i] 
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
  
  populationDT <- reactive({
    # Update only when update button is clicked 
    input$goButton
    
    N <- input$N
    
    #simulate the population
    population <- isolate(simPop(N = N))
    return(population)
  })
  
  Matrix <- reactive({
    # Update only when update button is clicked 
    input$goButton
    # read, and adjust if prompted, the transmission probability matrix dataframe

    if(input$adjustMatrix){
      # subtract the new desired offset
      transProbMatrix <- transProbMatrix-input$adjustMatrixValue
      # Probability has be greater than zero
      transProbMatrix[transProbMatrix < 0] <- 0
    }
    return(transProbMatrix)
  })
  
  dataTable2 <- reactive({
    # Update only when update button is clicked 
    input$goButton
    
    # number of personas to simulate
    N <- input$N
    
    # simulate the population
    popDT <- isolate(simPop(N = N))
    
    # split it in two halves
    popDT.1 <- popDT[1:(nrow(popDT)/2)]
    popDT.2 <- popDT[(nrow(popDT)/2+1):nrow(popDT)]
    
    # exposure levels
    exp.level.minor    <- input$exp.level.minor
    exp.level.moderate <- input$exp.level.moderate
    exp.level.high     <- input$exp.level.high
    
    # obtain distribution of exposure levels
    exp.levels <- expLevels(exp.level.minor, exp.level.moderate, exp.level.high, N/2)
    
    transProbMatrix <- isolate(Matrix())
        
    simDT <- simDiseaseTrans2( popDT.1[1], popDT.2[1], exposure = exp.levels[1], probMatrix = transProbMatrix)
    simDT <- rbind(simDT, simDiseaseTrans2( popDT.2[1], popDT.1[1], exposure = exp.levels[1], probMatrix = transProbMatrix))
    
    for(i in 2:nrow(popDT.1)){
      simDT  <- rbind(simDT, simDiseaseTrans2( popDT.1[i], popDT.2[i], exposure = exp.levels[i], probMatrix = transProbMatrix))
      simDT  <- rbind(simDT, simDiseaseTrans2( popDT.2[i], popDT.1[i], exposure = exp.levels[i], probMatrix = transProbMatrix))
    }
    
    # recode exposure levels
    simDT[, expLevel := as.factor(expLevel)]
    levels(simDT$expLevel) <- c("Minor", "Moderate", "High")
    
    # recode homestate levels
    simDT[, homestate := as.factor(homestate)]
    levels(simDT$homestate) <- c("Low", "Moderate", "High")
    
    return(simDT)
  })
  
  output$distsPlot2 <- renderPlot({
    input$goButton
    simDT <- isolate(dataTable2())
    
    p1 <- ggplot(simDT, aes(x = as.factor(healthstatus))) + geom_bar(fill = "#B5DAFF") 
    p1 <- p1 + xlab("Health Status of Population") + ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n")
    p1 <- p1 + theme_bw()
    p1 <- p1 + commonTheme
    
    p2 <- ggplot(simDT, aes(x = as.factor(exposer.HS))) + geom_bar(fill = "#ee3344") 
    p2 <- p2 + xlab("Health Status of Exposers") + ylab("Frequency") + ggtitle("Distribution of Health Status of Exposers\n")
    p2 <- p2 + theme_bw()
    p2 <- p2 + commonTheme
    
    p3 <- ggplot(simDT, aes(x = expLevel)) + geom_bar(fill = "#66cccc") 
    p3 <- p3 + xlab("Exposure Levels") + ylab("Frequency") + ggtitle("Distribution of Exposure Levels\n")
    p3 <- p3 + theme_bw()
    p3 <- p3 + commonTheme
    
    p4 <- ggplot(simDT, aes(x = as.factor(postExpHS))) + geom_bar(fill = "#0000FF") 
    p4 <- p4 + xlab("Health Status of Population Post Exposure") + 
      ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n Post Exposure")
    p4 <- p4 + theme_bw()
    p4 <- p4 + commonTheme
    
    grid.arrange(p1, arrangeGrob(p3, p4, ncol = 2), ncol=1)  
  })
  
  output$popPlots <- renderPlot({
    input$goButton
    simDT <- isolate(dataTable2())
    
    p1 <- ggplot(simDT, aes(x = as.factor(healthstatus), fill = as.factor(cut2(age, cuts = c(20, 60))))) + geom_bar() 
    p1 <- p1 + xlab("Health Status of Population") + ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n")
    p1 <- p1 + theme_bw()
    p1 <- p1 + theme(legend.position="bottom")
    p1 <- p1 + scale_fill_manual(values=c("#deebf7", "#9ecae1", "#3182bd"), 
                                 name="Population Age: ")
    p1 <- p1 + commonTheme
    
    p2 <- ggplot(simDT, aes(x = as.factor(healthstatus), fill = as.factor(homestate))) + geom_bar() 
    p2 <- p2 + xlab("Health Status of Population") + ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n")
    p2 <- p2 + theme_bw()
    p2 <- p2 + theme(legend.position="bottom")
    p2 <- p2 + scale_fill_manual(values=c("#e7e1ef", "#c994c7", "#dd1c77"), 
                                 name="Home-State Google Flu Trends: ")
    p2 <- p2 + commonTheme
    
    grid.arrange(p1, p2, ncol = 1)
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
  output$population <- renderDataTable({
    input$goButton
    popDT <- isolate(populationDT())
    popDT[, hascomorbidity := NULL]
    setnames(popDT, names(popDT), c('ID', 'Age', 'Health Status', 'Home-State Google Flu-Trend Level',
                                    'Comorbidity'))
  })
  
  output$dataTable2 <- renderDataTable({
    input$goButton
    simDT <- isolate(dataTable2())
    simDT[, hascomorbidity := NULL]
    setnames(simDT, names(simDT), c("Recipient ID", "Recipient Age", "Recipient Health Status", 
                                    "Recipient Home-state Google Flu-Trend Level",
                                    #"Recipient Has Morbiditiy", 
                                    "Recipient Morbidity", "Exposer ID", "Exposer Health Status", 
                                    "Exposure Level", "Probability", "Recipient Post-Exposure HS"))
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
  
  output$downloadPopData <- downloadHandler(
    filename = function() { paste('HP_HIMSS_Simulated_Population.csv', sep='') },
    content = function(file) {
      write.csv(populationDT(), file, row.names = FALSE)
    }
  )
  
  output$downloadIntData <- downloadHandler(
    filename = function() { paste('HP_HIMSS_Simulated_Interactions.csv', sep='') },
    content = function(file) {
      write.csv(dataTable2(), file, row.names = FALSE)
    }
  )
  
  output$downloadMatrix <- downloadHandler(
    filename = function() { paste('HP_HIMSS_Disease_Transmission_Matrix.csv', sep='') },
    content = function(file) {
      write.csv(Matrix(), file, row.names = TRUE)
    }
  )
  
})