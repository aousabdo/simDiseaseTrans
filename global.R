# function to simulate transmission of disease for HPs HIMSS App
# Author: Dr. Alex Abdo <alex.abdo@hp.com>
# Feb. 2 2015

# start fresh
# rm(list=ls())

# initiate libraries
library(ggplot2)
library(data.table)

# source needed R scripts
source("./multiplot.R")

# read disease probabilty transfer matrix and store it in a dataframe
df           <- read.csv("disease_transmission_matrix_2.csv", header = FALSE)

# assign somewhat useful column and row names for the display in the shiny app
colnames(df) <- paste("Exposer HS ", rep(1:6, each = 3), " EL: ", rep(c(" Minor ", " Moderate ", " High"),3), sep = "")
rownames(df) <- paste("Recipient HS ", rep(1:6), sep = "")


# function to simulate the spread of the disease
simDiseaseTrans <- function(recipient = 3, exposer = 5, exposure = 2, encounter = 3){
  # recipient and exposer take values between 1 and 6
  if(recipient < 1 | recipient > 6){stop("Recipient Health Status has to be in the range 1-6")}
  if(exposer  < 1 | exposer > 6){stop("Exposer Health Status has to be in the range 1-6")}
  
  # exposer take values between 1, minor exposure, and 3, high exposure
  if(exposure < 1 | exposure > 3){stop("Exposure level has to be in the range 1-3")}
  
  # number of encounters is bounded in the 1-3 range
  if(! (1 <= encounter & encounter <= 10)){stop("Number of encounters has to be in the 1-10 range")}
  
  # read omega, likelihood of advancing to next level of health status from dataframe
  omega <- df[recipient, ((exposer-1)*3 + exposure)]
  
  # if omega is larger than a randomly selected number between 1 and a 100 then
  # the recipient will advance to the next level of health status (become sicker)
  # if not then the recipient stays at the same health status level
  if(sample(0:100, 1) < omega) recipient.new <- recipient + 1
  else recipient.new <- recipient
  
  #   recipient.new <- recipient
  #   
  #   for(i in 1:encounter){
  #     if(sample(0:100, 1) < omega) recipient.new <- recipient + 1
  #   }
  
  return(recipient.new)
}

# this function takes three levels of percentages and creates a random sample
expLevels <- function(l1, l2, l3, N){
  # all levels in one vector
  exp.levels <- c(rep(1, l1/100*N), rep(2, l2/100*N), rep(3, l3/100*N))
  
  # if sum of all levels is less than a hundred then recycle to avoid error
  if(sum(l1, l2, l3) < 100 | sum(l1, l2, l3) > 100){ 
    exp.levels <- sample(exp.levels, size = N, replace = TRUE)
  }
  else exp.levels <- sample(exp.levels)
  return(exp.levels)
}

# common theme for the ggplots
commonTheme <- theme(axis.text.x = element_text(angle=0, hjust=1, size = 14),
                     axis.title.x = element_text(face="bold", colour="black", size=16),
                     axis.text.y = element_text(angle=0, hjust=1, size = 14),
                     axis.title.y = element_text(face="bold", colour="black", size=16),
                     plot.title = element_text(size = 20))
