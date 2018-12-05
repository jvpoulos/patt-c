## Prepares Oregon Health Insurance Experiment (OHIE) data

# Libraries
library(foreign)

# Define data directory
ohie.data.directory <- paste0(repo.directory,"data/OHIE_Public_Use_Files/OHIE_Data")

# Import data
f <- file.path(ohie.data.directory, c("oregonhie_descriptive_vars.dta",
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
rm(ohie.data.directory,f,ohie.list)
saveRDS(ohie, paste0(repo.directory,"data/prepare-ohie.RData"))