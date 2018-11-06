## Run this script on SCF

# Set WD
setwd("~/Documents/stat215b-final-project")

# Load R workspace
load("analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("SuperLearner.R")

# Predict who is a complier in the control group
set.seed(42)
complier.mod <- SuperLearner(Y=insurance.ohie[treatment.ohie==1], 
                             X=X.ohie[treatment.ohie == 1,], 
                             SL.library=SL.library.class,
                             family="binomial")
complier.mod

# Store predictions
C.pscore <- predict(complier.mod, X.ohie, onlySL=TRUE)

# Output predictions as .txt file
write.table(C.pscore, "C.pscore.txt",  row.names=FALSE)