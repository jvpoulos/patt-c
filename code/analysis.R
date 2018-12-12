## Estimate conditional expectation of responses in RCT 
## Then, use response model to estimate population members' outcomes given their covariates.
## These estimates will be used to estimate the PATT.

load(paste0(repo.directory,"data/prepare-analysis.RData")) # result of prepare-analysis.R

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
Y.ohie <- na.omit(data.frame("any.visit"=any.visit,
                             "num.visit"=num.visit,# need to omit rows containing any NA
                    "any.out"=any.out,
                   "num.out"=num.out))

Y.nhis <- na.omit(data.frame("any.visit"=nhis.any.visit,
                             "num.visit"=nhis.num.visit,# need to omit rows containing any NA
                     "any.out"=nhis.any.out,
                     "num.out"=nhis.num.out))

## Train compliance model on RCT treated. Use model to predict P(insurance == 1|covariates) on controls. 
run.model <- FALSE
if(run.model){
  # Source SuperLearner
  source(paste0(repo.directory,"code/SuperLearner.R"))
  save.image(paste0(repo.directory,"data/analysis.RData"))
  source(paste0(repo.directory, "code/complier-mod.R"))
}

# Load Super Learner predictions for compliance model (complier-mod.R)
C.pscore <- read.table(paste0(repo.directory,"results/C.pscore.txt"), quote="\"", header=TRUE)

rct.compliers <- data.frame("treatment"=treatment.ohie,
                            "insurance"=insurance.ohie,
                            "C.pscore"=as.numeric(C.pscore$pred), # SL predictions in first column
                            "C.hat"=ifelse(as.numeric(C.pscore[[1]])>0.5,1,0),
                            "complier"=0)
rct.compliers$complier[rct.compliers$treatment==1 & rct.compliers$insurance==1] <- 1 # true compliers in the treatment group
rct.compliers$complier[rct.compliers$treatment==0 & rct.compliers$C.hat==1] <- 1 # predicted compliers from the control group
 
# Fit a regression to the compliers in the RCT
y.col <- 1:4
y.col.binary <- c(1,3)
y.col.num <- c(2,4)
Y.ohie.response <- Y.ohie[which(rct.compliers$complier==1),]
X.ohie.response <- data.frame("insurance"=insurance.ohie[which(rct.compliers$complier==1)],
                         X.ohie[which(rct.compliers$complier==1),])

# For computing unadjusted PATT
Y.ohie.response.unadj <- Y.ohie[which(rct.compliers$complier==1 | rct.compliers$complier==0),]
X.ohie.response.unadj <- data.frame("insurance"=insurance.ohie,
                                    X.ohie)

if(run.model){ 
  save.image(paste0(repo.directory,"data/analysis.RData"))
  source(paste0(repo.directory, "code/response-mod.R"))
}

load(paste0(repo.directory,"results/response-mod-binary.rda")) 
load(paste0(repo.directory,"results/response-mod-binary2.rda")) 

load(paste0(repo.directory,"results/response-mod-num.rda")) 
load(paste0(repo.directory,"results/response-mod-num2.rda")) 

# Use response model to estimate potential outcomes for population "compliers" on medicaid
nrt.tr.counterfactual <- cbind("insurance" = rep(1, length(which(insurance.nhis==1))),
                               X.nhis[which(insurance.nhis==1),])
nrt.ctrl.counterfactual <- cbind("insurance" = rep(0, length(which(insurance.nhis==1))),
                                 X.nhis[which(insurance.nhis==1),])

Y.hat.1 <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], predict(response.mod.binary[[i]], nrt.tr.counterfactual, onlySL = T)$pred,
                                             predict(response.mod.num[[i]], nrt.tr.counterfactual, onlySL = T)$pred)) # extract SL predictions
Y.hat.0 <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], predict(response.mod.binary[[i]], nrt.ctrl.counterfactual, onlySL = T)$pred,
                                             predict(response.mod.num[[i]], nrt.ctrl.counterfactual, onlySL = T)$pred))

# Compute PATT estimator
t.patt <- lapply(y.col, function (i) mean(Y.hat.1[[i]]) - mean(Y.hat.0[[i]]))

# Compute unadjusted PATT

nrt.tr.counterfactual.unadj <- cbind("insurance" = rep(1, length(which(insurance.nhis==1 | insurance.nhis==0))),
                                     X.nhis[which(insurance.nhis==1| insurance.nhis==0),])
nrt.ctrl.counterfactual.unadj <- cbind("insurance" = rep(0, length(which(insurance.nhis==1 | insurance.nhis==0))),
                                       X.nhis[which(insurance.nhis==1 | insurance.nhis==0),])

Y.hat.1.unadj <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], predict(response.mod.binary2[[i]], nrt.tr.counterfactual.unadj, onlySL = T)$pred,
                                                   predict(response.mod.num2[[i]], nrt.tr.counterfactual.unadj, onlySL = T)$pred))
Y.hat.0.unadj <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], predict(response.mod.binary2[[i]], nrt.ctrl.counterfactual.unadj, onlySL = T)$pred,
                                                   predict(response.mod.num2[[i]], nrt.ctrl.counterfactual.unadj, onlySL = T)$pred))

t.patt.unadj <- lapply(y.col, function (i) mean(Y.hat.1.unadj[[i]]) - mean(Y.hat.0.unadj[[i]]))

# Compute adjusted SATT using predicted values from response model for RCT compliers

rct.ctrl.counterfactual.adj <- cbind("insurance" = rep(0, length(which(insurance.ohie==1))),
                                       X.ohie[which(insurance.ohie==1),])

Y.hat.1.adj.rct <- lapply(colnames(Y.ohie), function (i) Y.ohie[[i]][which(insurance.ohie==1)])
Y.hat.0.adj.rct <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], predict(response.mod.binary[[i]], rct.ctrl.counterfactual.adj, onlySL = T)$pred,
                                                     predict(response.mod.num[[i]], rct.ctrl.counterfactual.adj, onlySL = T)$pred))

t.satt.adj <- lapply(y.col, function (i) mean(Y.hat.1.adj.rct[[i]]) - mean(Y.hat.0.adj.rct[[i]]))

# Compute SATE
rct.sate <- lapply(y.col, function (i) (mean(Y.ohie[[i]][which(treatment.ohie==1)]) - # Num. is ITT effect
                                             mean(Y.ohie[[i]][which(treatment.ohie==0)])) 
                   /mean(rct.compliers$complier[which(treatment.ohie==1)])) # Denom. is true RCT compliance rate

# Save workspace
save.image(paste0(repo.directory,"data/analysis.RData")) 