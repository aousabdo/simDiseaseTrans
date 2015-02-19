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

# read disease probabilty transfer matrix and store it in a dataframe
transProbMatrix  <- read.csv("disease_transmission_matrix_2.csv", header = FALSE)

# assign somewhat useful column and row names for the display in the shiny app
colnames(transProbMatrix) <- paste("Exposer HS ", rep(1:6, each = 3), " EL: ", rep(c(" Minor ", " Moderate ", " High"),3), sep = "")
rownames(transProbMatrix) <- paste("Recipient HS ", rep(1:6), sep = "")


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
    if(recVec[, homestate == 3] ) omega <- omega + 4
    else if(recVec[, homestate ==2 ]) omega <- omega + 2
    
    # add weight for age
    if(recVec[, age < 20] | recVec[, age >= 60]) omega <- omega + 4
    
    # cap probability at a 100. This might happen since we are adding lots of probabilities
    if(omega > 100) omega <- 100
  }
  
  # add exposer's ID and healthstatus level. Also add exposure level and probability omega
  recVec[, c("exposer.id", "exposer.HS", "expLevel", "omega") := list(expVec[, id], expVec[, healthstatus], exposure, omega)]

  # if omega is larger than a randomly selected number between 1 and a 100 then
  # the recipient will advance to the next level of health status (become sicker)
  # if not then the recipient stays at the same health status level
  if(sample(1:100, 1) <= omega) recVec[, postExpHS := healthstatus + 1]
  else recVec[, postExpHS := healthstatus]
  
  return(recVec)
}

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

# function to merge two vectors interchangebly
interchange2V <- function(v1, v2){
  as.vector(rbind(v1, v2))
}

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
  
  # google flu trend rank for home states 
  homestate <- sample(1:3, N, replace = TRUE)
  # We'll use data tables to store population attributes
  popDT <- data.table("id" = sample(1e4:9e4, N), "age" = age, "healthstatus" = healthstatus, "homestate" = homestate)
  
  # add columns for comorbidities using the two comorbidities functions 
  popDT[, "hascomorbidity" := sapply(healthstatus, hasComorbidityDist)]
  popDT[, "comorbidity" := sapply(hascomorbidity, comorbidity)]
  return(popDT)
}

# function to simulate the existance of comorbidity according to health status
hasComorbidityDist <- function(healthStatus){
  if(healthStatus == 1 | healthStatus == 2) hasComorbidity <- rbinom(n = 1, size = 1, prob = 0.18)
  else hasComorbidity <- rbinom(n = 1, size = 1, prob = 0.5)
  return(hasComorbidity)
}

# function to assign comorbidities 
comorbidity <- function(hasComorbidity){
  if(!hasComorbidity) comorbidity <- NA
  else comorbidity <- {
    sample(c("Diabetes", "CHF", "COPD", "Immunosuppressed", "Cancer",	"Renal", "Transplant", "Cystic Fibrosis"), 1)
  }
  return(comorbidity)
}

# comorbidities weight
comorbidityWeight <- list("Diabetes" = 1, "CHF" = 1, "COPD" = 2, "Immunosuppressed" = 4, 
                          "Cancer" = 4,  "Renal" = 4, "Transplant" = 5, "Cystic Fibrosis" = 4)


# common theme for the ggplots
commonTheme <- theme(axis.text.x = element_text(angle=0, hjust=1, size = 14),
                     axis.title.x = element_text(face="bold", colour="black", size=16),
                     axis.text.y = element_text(angle=0, hjust=1, size = 14),
                     axis.title.y = element_text(face="bold", colour="black", size=16),
                     plot.title = element_text(size = 20), 
                     legend.title = element_text(colour="black", size=16, face="bold"),
                     legend.text = element_text(colour="black", size = 16, face = "bold"))