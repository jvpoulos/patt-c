repo.directory <- "~/Dropbox/github/patt-noncompliance/"

source(paste0(repo.directory,"code/prepare-ohie.R"))

source(paste0(repo.directory, "data/NHIS/download-all-nhis-microdata.R")) # download NHIS data 2008-17
source(paste0(repo.directory,"code/prepare-nhis.R"))


source(paste0(repo.directory,"code/prepare-analysis.R"))