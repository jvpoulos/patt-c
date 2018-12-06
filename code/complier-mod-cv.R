## Run this script on server

options(mc.cores = 28)

# We need to set a different type of seed that works across cores.
# Otherwise the other cores will go rogue and we won't get repeatable results.
# This version is for the "multicore" parallel system in R.
set.seed(1, "L'Ecuyer-CMRG")

# Load R workspace
setwd("patt-noncompliance")
load("data/analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("code/SuperLearner.R")

# Get risk estimates with 10-fold CV
set.seed(42)
complier.mod.cv <- CV.SuperLearner(Y=insurance.ohie[treatment.ohie==1], 
                             X=X.ohie[treatment.ohie == 1,], 
                             SL.library=SL.library.class,
                             family="binomial",
                             V=10,
                             parallel = "multicore",
                             cvControl =list(V=10L)) 

summary(complier.mod.cv)

# Output latex table
print(toLatex(summary(complier.mod.cv)))

# Output plot
plot(complier.mod.cv)
ggsave(file="complier-mod-cv.pdf", width = 210, height = 297, units = "mm")