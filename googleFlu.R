library(data.table)
library(reshape2)
library(Hmisc)

options(warn = -1)
googleFlu <- function(googleFluFileLink = "https://www.google.org/flutrends/us/data.txt", update = TRUE){
  # googleFluFileLink is a pointer to the google flu trend text file
  # this function downloads the latest text file, gets values for the last week of data, 
  # converts it into a .csv file and saves it
  # the output could also be piped to an object as a data.table
  # example of how to run the function:
  # googleFlu(update = TRUE)
  # flu = googleFlu(update = FASLSE)


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
  flu <- flu[ nrow(flu), -c(2, 54:ncol(flu)), with = FALSE]
  
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
  write.csv(flu, paste("googleFluTrend_",flu[1,Date] , ".csv", sep=""), row.names = FALSE)
  
  return(flu)
}