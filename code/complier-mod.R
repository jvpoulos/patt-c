## Run this script on server

setwd("patt-noncompliance")
# Load R workspace
load("data/analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("code/SuperLearner.R")

# Predict who is a complier in the control group
set.seed(42)
complier.mod <- SuperLearner(Y=insurance.ohie[treatment.ohie==1], 
                             X=X.ohie[treatment.ohie == 1,], 
                             SL.library=SL.library.class,
                             family="binomial")

summary(complier.mod)

# Store predictions
C.pscore <- predict(complier.mod, X.ohie, onlySL=TRUE)

# Output predictions as .txt file
write.table(C.pscore, "results/C.pscore.txt",  row.names=FALSE)

# Save model
saveRDS(complier.mod, "results/complier-mod.rda")