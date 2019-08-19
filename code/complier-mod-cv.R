## Run this script on server

options(mc.cores = 28)

# We need to set a different type of seed that works across cores.
# Otherwise the other cores will go rogue and we won't get repeatable results.
# This version is for the "multicore" parallel system in R.
set.seed(1, "L'Ecuyer-CMRG")

# Load R workspace
load("data/analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("code/SuperLearner.R")

# Get risk estimates with 10-fold CV
set.seed(42)
complier.mod.cv <- SuperLearner(Y=insurance.ohie[which(treatment.ohie==1)], 
                             X=X.ohie[which(treatment.ohie == 1),], 
                             SL.library=SL.library.class,
                             family="binomial",
                             parallel = "multicore",
                             cvControl =list(V=10L),
                             id=ohie.hhid[which(treatment.ohie == 1)],# force observations in the same cluster to be in the same validation fold
                             obsWeights = ohie.weights[which(treatment.ohie == 1)]) # observation weights

summary(complier.mod.cv)

# Save models
save(complier.mod.cv, file = "results/complier-mod-cv.rda")