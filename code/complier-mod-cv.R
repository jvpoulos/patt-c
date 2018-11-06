## Run this script on SCF

# Set WD
setwd("~/Documents/stat215b-final-project")

# Load R workspace
load("analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("SuperLearner.R")

# Get risk estimates with 10-fold CV
set.seed(42)
complier.mod.cv <- CV.SuperLearner(Y=insurance.ohie[treatment.ohie==1], 
                             X=X.ohie[treatment.ohie == 1,], 
                             SL.library=SL.library.class,
                             family="binomial",
                             V=10,
                             cvControl =list(V=10L)) 

summary(complier.mod.cv)

# Output latex table
print(toLatex(summary(complier.mod.cv)))

# Output plot
plot(complier.mod.cv)
ggsave(file="complier-mod-cv.pdf", width = 210, height = 297, units = "mm")