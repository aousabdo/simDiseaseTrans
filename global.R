# Helper file for the simulation of the  transmission of disease for HPs HIMSS App
# Author: Dr. Alex Abdo <alex.abdo@hp.com>
# Feb. 2 2015

# start fresh
# rm(list=ls())

# initiate libraries
library(ggplot2)
library(data.table)
library(gridExtra)
library(Hmisc)
library(gdata)
library(reshape2)
library(tools)
library(maps)
library(mapproj)

# -------------------------------------------------------------------------------------#
# read disease probabilty transfer matrix and store it in a dataframe
transProbMatrix  <- read.csv("disease_transmission_matrix_2.csv", header = FALSE)

# -------------------------------------------------------------------------------------#
# assign somewhat useful column and row names for the display in the shiny app
colnames(transProbMatrix) <- paste("Exposer HS ", rep(1:6, each = 3), " EL: ", 
                                   rep(c(" Minor ", " Moderate ", " High"),3), sep = "")
rownames(transProbMatrix) <- paste("Recipient HS ", rep(1:6), sep = "")
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# function to simulate the spread of the disease
simDiseaseTrans <- function(recipient = 3, exposer = 5, exposure = 2, homeState = 2, probMatrix){
  # recipient and exposer take values between 1 and 6
  if(recipient < 1 | recipient > 6){stop("Recipient Health Status has to be in the range 1-6")}
  if(exposer  < 1 | exposer > 6){stop("Exposer Health Status has to be in the range 1-6")}
  
  # exposer take values between 1, minor exposure, and 3, high exposure
  if(exposure < 1 | exposure > 3){stop("Exposure level has to be in the range 1-3")}
  
  # read omega, likelihood of advancing to next level of health status from dataframe
  omega <- probMatrix[recipient, ((exposer-1)*3 + exposure)]
  #   omega <- omega + homeStateWeight[[homeState, 2]]
  
  # if omega is larger than a randomly selected number between 1 and a 100 then
  # the recipient will advance to the next level of health status (become sicker)
  # if not then the recipient stays at the same health status level
  if(sample(1:100, 1) <= omega) recipient.new <- recipient + 1
  else recipient.new <- recipient
  
  if(recipient.new >6 ) recipient.new = 6
  return(recipient.new)
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
simDiseaseTrans2 <- function(recVec, expVec, exposure, probMatrix){
  # recVec: vector containing attributes for recipient in an interaction
  # expVec: vector containing attributes for exposer in an interaction
  # these two vectors contain the following attributes: id, age, homestate google flu trend level, health status
  # does the persona have comorbidity, if it has comorbidity what kind of comorbidity
  # first we'll get the base probability then add weights for modifiers
  # read disease transmission matrix for base probability
  omega <- probMatrix[recVec[, healthstatus], ((expVec[, healthstatus]-1)*3 + exposure)]
  
  # add weights for modifiers. We'll only add these for health status less than 6
  if(recVec[, healthstatus < 6]){
    # now add the weight for comorbidity if it exists
    if(recVec[, hascomorbidity]) omega <- omega + comorbidityWeight[[recVec[, comorbidity]]]
    
    # add weight for home state google flue trend 
    if(recVec[, homestate == "High"] ) omega <- omega + 4
    else if(recVec[, homestate == "Moderate" ]) omega <- omega + 2
    
    # add weight for age
    if(recVec[, age < 20] | recVec[, age >= 60]) omega <- omega + 4
    
    # cap probability at a 100. This might happen since we are adding lots of probabilities
    if(omega > 100) omega <- 100
  }
  
  # add exposer's ID and healthstatus level. Also add exposure level and probability omega
  recVec[, c("exposer.id", "exposer.HS", "expLevel", "probability", "changed") := 
            list(expVec[, id], expVec[, healthstatus], exposure, omega, "No")]
  
  # if omega is larger than a randomly selected number between 1 and a 100 then
  # the recipient will advance to the next level of health status (become sicker)
  # if not then the recipient stays at the same health status level
  if(sample(1:100, 1) <= omega) {
    recVec[, postExpHS := healthstatus + 1]
    recVec[, changed := "Yes"]
  }
  else recVec[, postExpHS := healthstatus]
  
  recVec[, changed := as.factor(changed)]
  
  return(recVec)
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# this function takes three levels of percentages and creates a random sample of a given length 
# using these percentages
expLevels <- function(l1, l2, l3, N){
  # all levels in one vector
  exp.levels <- c(rep(1, l1/100*N), rep(2, l2/100*N), rep(3, l3/100*N))
  # for odd N/2 we need to add the last item to the vector, defaulting this to 2
  if(length(exp.levels) < N) exp.levels[N] <- 2
  
  # if sum of all levels is different than a hundred then spit out an error
  if(sum(l1, l2, l3) < 100 | sum(l1, l2, l3) > 100){ 
    stop("Percentages of exposure levels have to add up to a 100")
  }
  return(sample(exp.levels))
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# function to merge two vectors interchangebly
interchange2V <- function(v1, v2){
  as.vector(rbind(v1, v2))
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# function to simulate the population with modifiers
simPop <- function(N = 100){
  # simulate the population, population is distributed in health statuses according to 
  # precentages from Dawn. The hard coded percentages used in the following distributio 
  # are taken from the excel spreadsheet provided by Bo
  healthstatus <- c(rep(1:4, N*0.2) ,rep(5:6, N*0.1))
  # randomize the population  
  healthstatus <- sample(healthstatus)
  
  # distribution of population ages
  age <- c(sample(10:20, 0.25*N, replace = TRUE), sample(21:60, 0.55*N, replace = TRUE), sample(61:81, 0.2*N, replace = TRUE))
  
  # read google flu data file and use it to simulate the states of participants
  flu         <- googleFlu()
  
  # sample the given number from google data
  homestate   <- flu[sample(1:nrow(flu), N, replace = TRUE)]
  
  # variables to add to the table, the state of the persona and its google flu level 
  flu         <- googleFlu()
  homestate   <- flu[sample(1:nrow(flu), N, replace = TRUE)]
  state       <- homestate[, state]
  homestateFL <- homestate[, fluLevel]
  
  # We'll use data tables to store population attributes
  popDT <- data.table("id" = sample(1e4:9e4, N), "age" = age, "healthstatus" = healthstatus, 
                      "homestate" = homestateFL, "state" = state)
  
  # add columns for comorbidities using the two comorbidities functions 
  popDT[, "hascomorbidity" := sapply(healthstatus, hasComorbidityDist)]
  popDT[, "comorbidity" := as.factor(sapply(hascomorbidity, comorbidity))]
  return(popDT)
}
# -------------------------------------------------------------------------------------#


# -------------------------------------------------------------------------------------#
simInteraction <- function(population, exp.levels = NULL, probMatrix = NULL, Modifiers = TRUE){
  # function to simulate the interaction between personas in a population popDT
  # split given population in halves
  popDT.1 <- population[1:(nrow(population)/2)] # this will be the recipients data.table
  popDT.2 <- population[(nrow(population)/2+1):nrow(population)] # this will be the exposers data.tables
  
  # change names of recipient and exposers columns
  setnames(popDT.1, paste('rec', names(popDT.1), sep = "."))
  setnames(popDT.2, paste('exp', names(popDT.2), sep = "."))
  
  # bind two data.table in one.
  tmp.1 <- cbind(popDT.1, popDT.2)
  
  tmp.1[, exposure := exp.levels]
  
  # now we need to take care of the two way interaction
  # change names of recipient and exposers columns
  setnames(popDT.1, gsub('rec', 'exp', names(popDT.1)))
  setnames(popDT.2, gsub('exp', 'rec', names(popDT.2)))
  
  # add second-way interaction
  tmp.2 <- cbind(popDT.2, popDT.1)
  tmp.2[, exposure := exp.levels]

  # interleave two data.tables in one
  pop.tmp <- interleave(tmp.1, tmp.2)

  # add probability
  pop.tmp[, probability := as.numeric(diag(sapply(pop.tmp$rec.healthstatus, readMatrix,
                                                        expHS = pop.tmp$exp.healthstatus,
                                                        exposure = pop.tmp$exposure, 
                                                        probMatrix= probMatrix)))]
  
  pop.tmp[rec.healthstatus == 6, probability := 0]
  
  result <- copy(pop.tmp)
  
  if(Modifiers){
    result <- modifiers(pop.tmp)
  }
  
  result[, c("changed", "rec.HS.PostExp") := list("No", rec.healthstatus)]
  result[sample(1:100, nrow(result), replace = TRUE) <= probability, 
         c("changed","rec.HS.PostExp") := list("Yes", rec.healthstatus + 1)]
  
  factors <- c("rec.homestate", "rec.comorbidity", "exp.comorbidity", "exposure", "changed")
  
  result[, (factors):=lapply(.SD, as.factor), .SDcols=factors]
  
  levels(result$exposure) <- c("Minor", "Moderate", "High")
  result$rec.homestate <- reorder(result$rec.homestate, new.order= c("Low", "Moderate", "High"))
  
  return(result)
}
# -------------------------------------------------------------------------------------#

modifiers <- function(population){
  # make sure you use copy since othewise parent 
  # data.table get affected when it's copy is altered
  population.tmp <- copy(population) 
  
  # add weight for home state google flue trend 
  population.tmp[rec.homestate == "High", probability := probability + 4]
  population.tmp[rec.homestate == "Moderate", probability := probability + 2]
  
  # add weight for age    
  population.tmp[rec.age <= 20 | rec.age >=60, probability := probability + 4]
  
  # now add the weight for comorbidity if it exists
  population.tmp[rec.hascomorbidity == 1, 
                 probability := probability + comorbidityweightDT[comorbidity == as.character(population.tmp[, rec.comorbidity]), weights]
                 ]
  
  # cap probability at a 100. This might happen since we are adding lots of probabilities
  population.tmp[probability > 100, probability := 100]
  
  # HS 6 probability is zero
  population.tmp[rec.healthstatus == 6, probability := 0]
  
  return(population.tmp)
}

# -------------------------------------------------------------------------------------#
# function to read 
readMatrix <- function(recHS, expHS, exposure, probMatrix){
  probMatrix[recHS, ((expHS-1)*3 + exposure)]
}
# -------------------------------------------------------------------------------------#


# -------------------------------------------------------------------------------------#
# function to simulate the existance of comorbidity according to health status
hasComorbidityDist <- function(healthStatus){
  if(healthStatus == 1 | healthStatus == 2) hasComorbidity <- rbinom(n = 1, size = 1, prob = 0.18)
  else hasComorbidity <- rbinom(n = 1, size = 1, prob = 0.5)
  return(hasComorbidity)
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# function to assign comorbidities 
comorbidity <- function(hasComorbidity){
  if(!hasComorbidity) comorbidity <- NA
  else comorbidity <- {
    sample(c("Diabetes", "CHF", "COPD", "Immunosuppressed", "Cancer",	"Renal", "Transplant", 
             "Cystic Fibrosis"), 1)
  }
  return(comorbidity)
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# comorbidities weight
comorbidityWeight <- list("Diabetes" = 1, "CHF" = 1, "COPD" = 2, "Immunosuppressed" = 4, 
                          "Cancer" = 4,  "Renal" = 4, "Transplant" = 5, "Cystic Fibrosis" = 4)

comorbidityweightDT <- data.table(comorbidity = c("Diabetes" , "CHF" , "COPD" , 
                                                  "Immunosuppressed", 
                                                  "Cancer",  "Renal", 
                                                  "Transplant", "Cystic Fibrosis"), 
                                  weights = c(1, 1, 2, rep(4,3), 5, 4))
setkey(comorbidityweightDT, comorbidity)
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
googleFlu <- function(googleFluFileLink = "https://www.google.org/flutrends/us/data.txt", 
                      update = FALSE){
  # googleFluFileLink is a pointer to the google flu trend text file
  # this function downloads the latest text file, gets values for the last week of data, 
  # converts it into a .csv file and saves it
  
  # if file exists and if update is false then don't redownload file
  if(file.exists("./googleFluFile.txt")){
    if(update){
      # download file 
      cat("Downloading File ....\n")
      download.file(url = googleFluFileLink, destfile = "./googleFluFile.txt")
    }
  }#otherwise download file
  else{
    cat("Downloading File ....\n")
    # download file 
    download.file(url = googleFluFileLink, destfile = "./googleFluFile.txt")
  }
  
  # fread will take care of the few, 11 lines, of text and will only read the data columns
  flu <- fread("./googleFluFile.txt")
  
  # keep only data for the last week (last row), also get rid of some columns that we don't need
  flu <- flu[ nrow(flu), -c(1:2, 54:ncol(flu)), with = FALSE]
  
  # also get rid of Hawaii and Alaska since R can't plot them
  flu[,c("Alaska", "Hawaii") := list(NULL, NULL)]
  
  # convert to long format
  flu <- melt(flu, variable.name = "state", value.name = "count")
  
  # let's add a categorical variable based on the counts
  flu[, fluLevel := as.factor(cut2(count, g = 3))]
  levels(flu$fluLevel) <- c("Low", "Moderate", "High")
  
  
  # set data.table key to count to sort the data.table by flu counts
  setkey(flu, count)
  
  # save data.table to a csv file
  write.csv(flu, "googleFluTrend.csv", row.names = FALSE)
  
  return(flu)
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# function to make a choropleth map of google flu trends
fluMap <- function(fluData, popDT){
  # This function makes two maps: google flu trend map and 
  # a choropleth map of states where our participants came from
  
  # make the google flu trned map first
  # transfer names of states to lower caps
  fluData[, state := tolower(state)]
  # set key for merge with maps data
  setkey(fluData, state)
  
  # build states map data
  states_map <- as.data.table(map_data('state'))
  
  # rename column for merge
  states_map[, state := as.factor(region)]
  states_map[, region := NULL]
  
  # set key for merge
  setkey(states_map, state)
  
  flu_map <- merge(states_map, fluData)
  
  p1 <- ggplot(flu_map, aes(x = long, y = lat, group = group, fill = count)) + geom_polygon(col="black") 
  p1 <- p1 + theme_bw() + theme(legend.position = "none", line = element_blank()) + coord_map("polyconic") 
  p1 <- p1 + scale_fill_continuous(low="yellow", high="red") + commonTheme
  #   p1 <- p1 + scale_fill_manual(name = "Number of Participants\t",values=c("#999999", "#E69F00", "#56B4E9"))
  p1 <- p1 + ggtitle("Google Flu Trends\n")
  p1 <- p1 + annotate("text", x = -95, y = 25, size = 4, colour = "blue", 
                      label = "Data Source: Google Flu Trends (http://www.google.org/flutrends)")
  p1 <- p1 + theme(axis.text.x = element_text(colour = "white"),
                   axis.title.x = element_text(colour="white"),
                   axis.text.y = element_text(colour = "white"),
                   axis.title.y = element_text(colour="white"),
                   plot.title = element_text(size = 20, colour = "blue", face = "bold"))
  
  # now build the population map
  popDT[, state := tolower(state)]
  
  # make a data.table for state occurances
  stateDT <- as.data.table(table(popDT$state))
  names(stateDT) <- c("state", "personas.from.state")
  
  # set key for merge with maps data
  setkey(stateDT, state)   
  
  # merge with state map data
  persona_map <- merge(states_map, stateDT, all.x = TRUE) # we are keeping all states
  
  # reset NAs to zeros for missing states
  set(persona_map, which(is.na(persona_map[[7]])), j = 7, value = 0)
  
  p2 <- ggplot(persona_map, aes(x = long, y = lat, group = group, fill = personas.from.state)) + geom_polygon(col="black") 
  p2 <- p2 + theme_bw() + theme(legend.position = "bottom", line = element_blank()) + coord_map("polyconic") 
  p2 <- p2 + scale_fill_gradient(name = "Number of Participants\t", low="white", high="red") + commonTheme
  p2 <- p2 + ggtitle("Map of where our Participants came from\n")
  p2 <- p2 + theme(axis.text.x = element_text(colour = "white"),
                   axis.title.x = element_text(colour="white"),
                   axis.text.y = element_text(colour = "white"),
                   axis.title.y = element_text(colour="white"),
                   plot.title = element_text(size = 20, colour = "blue", face = "bold"))  
  grid.arrange(p1, p2, ncol=1)
}
# -------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------#
# common theme for the ggplots
commonTheme <- theme(axis.text.x = element_text(angle=0, hjust=1, size = 14),
                     axis.title.x = element_text(face="bold", colour="black", size=16),
                     axis.text.y = element_text(angle=0, hjust=1, size = 14),
                     axis.title.y = element_text(face="bold", colour="black", size=16),
                     plot.title = element_text(size = 20), 
                     legend.title = element_text(colour="black", size=16, face="bold"),
                     legend.text = element_text(colour="black", size = 16, face = "bold"))
# -------------------------------------------------------------------------------------#