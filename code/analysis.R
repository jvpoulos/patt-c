## Estimate conditional expectation of responses in RCT 
## Then, use response model to estimate population members' outcomes given their covariates.
## These estimates will be used to estimate the PATT.

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source scripts
run.source <- FALSE
if(run.source){
source(file.path(directory,"prepare-analysis.R"))
save.image(file.path(directory,"prepare-analysis.Rdata"))
}
load(file.path(directory,"prepare-analysis.Rdata")) # result of prepare-analysis.R

# Create dfs containing common features for RCT and observational study
X.ohie <- na.omit(data.frame(n.hh,  # need to omit rows containing any NA
                             gender, 
                             age.19to49,
                             age.50to64,
                             white,
                             black,
                             hisp,
                             diabetes,
                             asthma,
                             bp,
                             heart,
                             education,
                             income)) 

X.nhis <-   na.omit(data.frame(n.hh.nhis, # need to omit rows containing any NA
                             gender.nhis, 
                             "age.19to49"=age.19to49.nhis,
                             "age.50to64"=age.50to64.nhis,
                             "white"=white.nhis,
                             "black"=black.nhis,
                             "hisp"=hisp.nhis,
                             "diabetes"=diabetes.nhis,
                             "asthma"=asthma.nhis,
                             "bp"=bp.nhis,
                             "heart"=heart.nhis,
                             education.nhis,
                             income.nhis))

# Create vectors for treatment and compliance 
treatment.ohie <- treatment[as.numeric(rownames(X.ohie))]

insurance.ohie <- insurance[as.numeric(rownames(X.ohie))]
insurance.nhis <- medicaid[as.numeric(rownames(X.nhis))]

# Create dfs for outcomes 
Y.ohie <- na.omit(data.frame("any.visit"=any.visit, # need to omit rows containing any NA
                    "any.out"=any.out))

Y.nhis <- na.omit(data.frame("any.visit"=nhis.any.visit, # need to omit rows containing any NA
                     "any.out"=nhis.any.out))

## Train compliance model on RCT treated. Use model to predict P(insurance == 1|covariates) on controls. 
run.model <- FALSE
if(run.model){ # Run compliance model on SCF
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
}

# Load Super Learner predictions for compliance model (complier-mod.R)
C.pscore <- read.table(paste0(directory,"/C.pscore.txt"), quote="\"") 

rct.compliers <- data.frame("treatment"=treatment.ohie,
                            "insurance"=insurance.ohie,
                            "C.pscore"=as.numeric(C.pscore[[1]]), # SL predictions in first column
                            "C.hat"=ifelse(as.numeric(C.pscore[[1]])>=0.5,1,0),
                            "complier"=0)
rct.compliers$complier[rct.compliers$treatment==1 & rct.compliers$insurance==1] <- 1 # true compliers in the treatment group
rct.compliers$complier[rct.compliers$treatment==0 & rct.compliers$C.hat==1] <- 1 # predicted compliers from the control group
 
# Fit a regression to the compliers in the RCT
y.col <- 1:ncol(Y.ohie) # number of responses
Y.ohie.response <- Y.ohie[which(rct.compliers$complier==1),]
X.ohie.response <- data.frame("treatment"=treatment.ohie[which(rct.compliers$complier==1)],
                         X.ohie[which(rct.compliers$complier==1),])

if(run.model){ 
# Run response model
set.seed(42)
response.mod <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response[,i], 
                                                        X=X.ohie.response, 
                                                        SL.library=SL.library.class,
                                                        family="binomial"))

names(response.mod) <- colnames(Y.ohie.response) # name each element of list

response.mod # summarize

save(response.mod, file = "response.mod.rda") # save model
}

load(file.path(directory,"response.mod.rda")) # result of response-mod.R

# Use response model to estimate potential outcomes for population "compliers" on medicaid
nrt.tr.counterfactual <- cbind("treatment" = rep(1, length(which(insurance.nhis==1))),
                               X.nhis[which(insurance.nhis==1),])
nrt.ctrl.counterfactual <- cbind("treatment" = rep(0, length(which(insurance.nhis==1))),
                                 X.nhis[which(insurance.nhis==1),])

Y.hat.1 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.tr.counterfactual)$library.predict[,8]) # extract RF predictions
Y.hat.0 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.ctrl.counterfactual)$library.predict[,8])

# Compute PATT estimator
t.patt <- lapply(y.col, function (i) mean(Y.hat.1[[i]]) - mean(Y.hat.0[[i]]))

# Compute unadjusted PATT
Y.ohie.response.unadj <- Y.ohie[which(rct.compliers$complier==1 | rct.compliers$complier==0),]
X.ohie.response.unadj <- data.frame("treatment"=treatment.ohie,
                                    X.ohie)

if(run.model){ 
set.seed(42)
response.mod2 <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response.unadj[,i], 
                                                         X=X.ohie.response.unadj, 
                                                         SL.library=SL.library.class,
                                                         family="binomial"))

names(response.mod2) <- colnames(Y.ohie) # name each element of list

response.mod2 # summarize

save(response2.mod, file = "response.mod2.rda") # save model
}

load(file.path(directory,"response.mod2.rda")) # result of response-mod.R

nrt.tr.counterfactual.unadj <- cbind("treatment" = rep(1, length(which(insurance.nhis==1 | insurance.nhis==0))),
                                     X.nhis[which(insurance.nhis==1| insurance.nhis==0),])
nrt.ctrl.counterfactual.unadj <- cbind("treatment" = rep(0, length(which(insurance.nhis==1 | insurance.nhis==0))),
                                       X.nhis[which(insurance.nhis==1 | insurance.nhis==0),])

Y.hat.1.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.tr.counterfactual.unadj)$library.predict[,8]) # extract RF predictions
Y.hat.0.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.ctrl.counterfactual.unadj)$library.predict[,8])

t.patt.unadj <- lapply(y.col, function (i) mean(Y.hat.1.unadj[[i]]) - mean(Y.hat.0.unadj[[i]]))

# Compute unadjusted SATT
rct.tr.counterfactual.unadj <- cbind("treatment" = rep(1, length(which(insurance.ohie==1 | insurance.ohie==0))),
                                     X.ohie[which(insurance.ohie==1| insurance.ohie==0),])
rct.ctrl.counterfactual.unadj <- cbind("treatment" = rep(0, length(which(insurance.ohie==1 | insurance.ohie==0))),
                                       X.ohie[which(insurance.ohie==1 | insurance.ohie==0),])

Y.hat.1.unadj.rct <- lapply(y.col, function (i) predict(response.mod2[[i]], rct.tr.counterfactual.unadj)$library.predict[,8]) # extract RF predictions
Y.hat.0.unadj.rct <- lapply(y.col, function (i) predict(response.mod2[[i]], rct.ctrl.counterfactual.unadj)$library.predict[,8])

t.satt.unadj <- lapply(y.col, function (i) mean(Y.hat.1.unadj.rct[[i]]) - mean(Y.hat.0.unadj.rct[[i]]))

# Compute SATE
rct.sate <- lapply(y.col, function (i) (mean(Y.ohie[[i]][which(treatment.ohie==1)]) - # Num. is ITT effect
                                             mean(Y.ohie[[i]][which(treatment.ohie==0)])) 
                   /mean(rct.compliers$complier[which(treatment.ohie==1)])) # Denom. is true RCT compliance rate

# Save workspace
save.image(file.path(directory,"analysis.Rdata"))


