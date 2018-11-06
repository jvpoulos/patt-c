## Run this script on SCF

# Set WD
setwd("~/Documents/stat215b-final-project")

# Load R workspace
load("analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("SuperLearner.R")

# Fit a regression to the compliers in the RCT
y.col <- 1:ncol(Y.ohie) # number of responses
Y.ohie.response <- Y.ohie[which(rct.compliers$complier==1),]
X.ohie.response <- data.frame("treatment"=treatment.ohie[which(rct.compliers$complier==1)],
                              X.ohie[which(rct.compliers$complier==1),])
# Run response model
set.seed(42)
response.mod <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response[,i], 
                                                        X=X.ohie.response, 
                                                        SL.library=SL.library.class,
                                                        family="binomial"))

names(response.mod) <- colnames(Y.ohie.response) # name each element of list

response.mod # summarize

# Compute unadjusted PATT
Y.ohie.response.unadj <- Y.ohie[which(rct.compliers$complier==1 | rct.compliers$complier==0),]
X.ohie.response.unadj <- data.frame("treatment"=treatment.ohie,
                                    X.ohie)
set.seed(42)
response.mod2 <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response.unadj[,i], 
                                                         X=X.ohie.response.unadj, 
                                                         SL.library=SL.library.class,
                                                         family="binomial"))

names(response.mod2) <- colnames(Y.ohie) # name each element of list

response.mod2 # summarize

# Save models
save(response.mod, file = "response.mod.rda")
save(response.mod2, file = "response.mod2.rda")

