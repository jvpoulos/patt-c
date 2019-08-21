## Run this script on server

# Load R workspace
load("data/analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("code/SuperLearner.R")

## For PATT-C

# Run response model
set.seed(42)
response.mod <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response[,i], 
                                                                      X=X.ohie.response, 
                                                                   SL.library=SL.library.reg,
                                                                   family="gaussian",
                                                                id=ohie.hhid[which(rct.compliers$complier==1)],
                                                                obsWeights = ohie.weights[which(rct.compliers$complier==1)]))

names(response.mod) <- colnames(Y.ohie.response)[y.col] # name each element of list

response.mod # summarize

## For PATT

# Run response model
set.seed(42)
response.mod.patt <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response.unadj[,i], 
                                                                       X=X.ohie.response.unadj, 
                                                                       SL.library=SL.library.reg,
                                                                       family="gaussian",
                                                                 id=ohie.hhid[which(rct.compliers$complier==1 | rct.compliers$complier==0)],
                                                                 obsWeights = ohie.weights[which(rct.compliers$complier==1 | rct.compliers$complier==0)])) 

names(response.mod.patt) <- colnames(Y.ohie)[y.col] # name each element of list

response.mod.patt # summarize

# Save models

save(response.mod, file = "results/response-mod.rda")
save(response.mod.patt, file = "results/response-mod-patt.rda")