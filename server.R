library(shiny)

shinyServer(function(input, output) {
  
  dataTable <- reactive({
    input$goButton
    
    # read inputs
    N         <- input$N # number of personas
    encounter <- input$encounter # number of encounters
    n1        <- vector(mode = 'numeric', length = N)
    n2        <- vector(mode = 'numeric', length = N) 
    
    simList <- list() # list to hold simulation data tables for different encounters
    value <- vector(mode = 'numeric', length = N)
    
    exp.level.minor    <- input$exp.level.minor
    exp.level.moderate <- input$exp.level.moderate
    exp.level.high     <- input$exp.level.high
    
    # obtain distribution of exposure levels
    exp.levels <- expLevels(exp.level.minor, exp.level.moderate, exp.level.high, N)
    
    # read, and adjust if prompted, the transmission probability matrix dataframe
    if(input$adjustMatrix){
      # subtract the new desired offset
      transProbMatrix <- transProbMatrix-input$adjustMatrixValue
      # P >= 0
      transProbMatrix[transProbMatrix < 0] <- 0
    }
    
    # distribution of simulated personas
    # for the same poplation we'll have recipients, n1, and exposers, n2.
    # they are the same but n2 is a resample of n1 to avoid same perosnas always
    # meeting with itself
    n1 <- c(rep(1:4, N*0.2) ,rep(5:6, N*0.1))
    n2 <- sample(n1)
    
    # for each encounter we need to obtain the health status of the reipients
    for(j in 1:encounter){
      for(i in 1:N){
        value[i] <- simDiseaseTrans(recipient = n1[i], exposer =  n2[i], 
                                    exposure = exp.levels[i], probMatrix = transProbMatrix)
      }
      # store into datatables inside the list
      simList[[j]] <- data.table("recipient" = n1, "exposer" = n2, "exposure" = exp.levels,  "recipient.new" = value)
      # now assign the new health status of recipients to the whole population
      n1 <- value
      # shuffle for exposers before rerunning the loop
      n2 <- sample(n1)
    }
    
    return(simList)
  })
  
  output$distsPlot <- renderPlot({  
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
    p4 <- p4 + xlab("Health Status of Recipients Post Exposure") + 
      ylab("Frequency") + ggtitle("Distribution of Health Status of Recipients\n Post Exposure")
    p4 <- p4 + theme_bw()
    p4 <- p4 + commonTheme
    
    multiplot(p1, p3, p2, p4, cols = 2)
  })
  
  output$postExpPlot <- renderPlot({ 
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
    input$goButton
    simDT <- isolate(dataTable()[[length(dataTable())]])
    
    simDT$exposure <- as.factor(simDT$exposure)
    levels(simDT$exposure) <- c("Minor", "Moderate", "High")
    setnames(simDT, names(simDT), 
             c("Health Status of Recipient", "Health Status of Exposer", "Exposure Level", 
               "Health Status of Recipient Post Exposure"))
  })
  
  output$transMatrix <- renderTable({
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
