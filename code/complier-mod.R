## Run this script on server

# Load R workspace
load("data/analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("code/SuperLearner.R")

# Predict who is a complier in the control group
set.seed(42)
complier.mod <- SuperLearner(Y=insurance.ohie[which(treatment.ohie==1)], 
                             X=X.ohie[which(treatment.ohie == 1),], 
                             SL.library=SL.library.class,
                             family="binomial",
                             id=ohie.hhid,# force observations in the same cluster to be in the same validation fold
                             obsWeights = ohie.weights[which(treatment.ohie == 1)]) # observation weights

summary(complier.mod)

# Store predictions
C.pscore <- predict(complier.mod, X.ohie, onlySL=TRUE)

# Output predictions as .txt file
write.table(C.pscore, "results/C.pscore.txt",  row.names=FALSE)

# Save model
save(complier.mod, file="results/complier-mod.rda")