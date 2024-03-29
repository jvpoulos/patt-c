## Prepares National Health Interview Survey (NHIS) data

# Libraries
library(foreach)
library(doParallel)

# Register cores for parallel processing
registerDoParallel(8)

# Set directory to NHIS data directory
setwd(paste0(repo.directory, "data/NHIS"))

# Create function to run merge script for all years
# Script merges person with adult sample files and takes the average 
# of imputed income vars across five imputed income files
nhisMerge <- function(i){
  year <- i
  source("merge-nhis.R", local=TRUE)
  return(x.sa)
}

years <- c(2008:2017) 

# Combine each merged dataset into list
nhis <- foreach(i=years) %dopar% { 
  data <- nhisMerge(i)
  return(data)
}

names(nhis) <- c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017") # name elements

# Clean up workspace
rm(nhisMerge)
saveRDS(nhis, paste0(repo.directory,"data/prepare-NHIS.RData"))