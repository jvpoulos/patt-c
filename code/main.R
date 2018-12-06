repo.directory <- "~/Dropbox/github/patt-noncompliance/"

source(paste0(repo.directory,"code/prepare-ohie.R")) # --> data/prepare-ohie.Rdata

#source(paste0(repo.directory, "data/NHIS/download-all-nhis-microdata.R")) # download NHIS data 2008-17
#source(paste0(repo.directory, "data/NHIS/2015/extract_nhis_2015.R")) 
#source(paste0(repo.directory, "data/NHIS/merge-nhis.R"))
source(paste0(repo.directory,"code/prepare-nhis.R")) #  script merges person, sample adult, and imputed income files --> data/prepare-nhis.RData

source(paste0(repo.directory,"code/prepare-analysis.R")) # loads data/prepare-ohie.Rdata and data/prepare-nhis.RData
                                                          # --> data/prepare-analysis.RData

source(paste0(repo.directory,"code/analysis.R"))