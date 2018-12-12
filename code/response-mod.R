## Run this script on server

# Load R workspace
load("data/analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("code/SuperLearner.R")

## For PATT-C

# Run response model (binary)
set.seed(42)
response.mod.binary <- lapply(y.col.binary, function (i) SuperLearner(Y=Y.ohie.response[,i], 
                                                        X=X.ohie.response, 
                                                        SL.library=SL.library.class,
                                                        family="binomial"))

names(response.mod.binary) <- colnames(Y.ohie.response)[y.col.binary] # name each element of list

response.mod.binary # summarize

# Run response model (num)
set.seed(42)
response.mod.num <- lapply(y.col.num, function (i) SuperLearner(Y=Y.ohie.response[,i], 
                                                                      X=X.ohie.response, 
                                                                   SL.library=SL.library.reg,
                                                                   family="gaussian"))

names(response.mod.num) <- colnames(Y.ohie.response)[y.col.num] # name each element of list

response.mod.num # summarize

## For PATT

# Run response model (binary)
set.seed(42)
response.mod.binary2 <- lapply(y.col.binary, function (i) SuperLearner(Y=Y.ohie.response.unadj[,i], 
                                                         X=X.ohie.response.unadj, 
                                                         SL.library=SL.library.class,
                                                         family="binomial"))

names(response.mod.binary2) <- colnames(Y.ohie)[y.col.binary] # name each element of list

response.mod.binary2 # summarize

# Run response model (num)
set.seed(42)
response.mod.num2 <- lapply(y.col.num, function (i) SuperLearner(Y=Y.ohie.response.unadj[,i], 
                                                                       X=X.ohie.response.unadj, 
                                                                       SL.library=SL.library.reg,
                                                                       family="gaussian"))

names(response.mod.num2) <- colnames(Y.ohie)[y.col.num] # name each element of list

response.mod.binary2 # summarize

# Save models
save(response.mod.binary, file = "results/response-mod-binary.rda")
save(response.mod.binary2, file = "results/response-mod-binary2.rda")

save(response.mod.num, file = "results/response-mod-num.rda")
save(response.mod.num2, file = "results/response-mod-num2.rda")
