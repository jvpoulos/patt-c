## Prepares Oregon Health Insurance Experiment (OHIE) data

# Libraries
library(foreign)

# Define data directory
data.directory <- "~/Dropbox/github/stat215b-final-project/data/OHIE_Public_Use_Files/OHIE_Data"

# Import data
f <- file.path(data.directory, c("oregonhie_descriptive_vars.dta",
                                 "oregonhie_ed_vars.dta",
                                 "oregonhie_inperson_vars.dta",
                                 "oregonhie_stateprograms_vars.dta",
                                 "oregonhie_survey0m_vars.dta",
                                 "oregonhie_survey12m_vars.dta"))
ohie.list <- lapply(f, read.dta) # read data to list
names(ohie.list) <- gsub(".*/oregonhie_(.*)\\..*", "\\1", f) # name elements

# Merge into single dataframe by unique person ID
ohie <- Reduce(function(...) merge(..., by="person_id", all=T), ohie.list)

# Clean up workspace
rm(data.directory,f,ohie.list)