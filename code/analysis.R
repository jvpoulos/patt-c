library(ROCR)

load(paste0(repo.directory,"data/prepare-analysis.RData")) # result of prepare-analysis.R

# Create dfs containing common features for RCT and observational study
X.ohie <- na.omit(data.frame(n.hh,  # need to omit rows containing any NA
                             n.children,
                             gender, 
                             age.19to49,
                             age.50to64,
                             white,
                             black,
                             asian,
                             aian,
                             race.other,
                             hisp,
                             diabetes,
                             asthma,
                             bp,
                             copd,
                             heart,
                             education,
                             income,
                             partner,
                             employed,
                             wave,
                             wave.interact)) 

X.nhis <-   na.omit(data.frame(n.hh.nhis, # need to omit rows containing any NA
                             n.children.nhis,
                             gender.nhis, 
                             "age.19to49"=age.19to49.nhis,
                             "age.50to64"=age.50to64.nhis,
                             "white"=white.nhis,
                             "black"=black.nhis,
                             "asian"=asian.nhis,
                             "aian"=aian.nhis,
                             "other"=race.other.nhis,
                             "hisp"=hisp.nhis,
                             "diabetes"=diabetes.nhis,
                             "asthma"=asthma.nhis,
                             "bp"=bp.nhis,
                             "copd"=copd.nhis,
                             "heart"=heart.nhis,
                             education.nhis,
                             income.nhis,
                             "partner"=partner.nhis,
                             "employed"=employed.nhis,
                             wave.nhis,
                             wave.nhis.interact))

colnames(X.nhis) <- colnames(X.ohie) # make sure names are consistent

# Create vectors for treatment and compliance 
treatment.ohie <- treatment[as.numeric(rownames(X.ohie))]

insurance.ohie <- insurance[as.numeric(rownames(X.ohie))]
insurance.nhis <- medicaid[as.numeric(rownames(X.nhis))]

# Create dfs for outcomes 
Y.ohie <- na.omit(data.frame("num.visit"=num.visit,# need to omit rows containing any NA
                   "num.out"=num.out))

Y.nhis <- na.omit(data.frame("num.visit"=nhis.num.visit,# need to omit rows containing any NA
                     "num.out"=nhis.num.out))

## Train compliance model on RCT treated. Use model to predict P(insurance == 1|covariates) on controls. 
run.model <- FALSE
if(run.model){ # run these scripts on server
  source(paste0(repo.directory,"code/SuperLearner.R"))
  save.image(paste0(repo.directory,"data/analysis.RData"))
  source(paste0(repo.directory, "code/complier-mod.R")) 
  source(paste0(repo.directory, "code/complier-mod-cv.R"))
}

# Load Super Learner predictions for compliance model (complier-mod.R)
C.pscore <- read.table(paste0(repo.directory,"results/C.pscore.txt"), quote="\"", header=TRUE)

rct.compliers <- data.frame("treatment"=treatment.ohie,
                            "insurance"=insurance.ohie,
                            "C.pscore"=C.pscore$pred,
                            "complier"=rep(0,length(as.numeric(treatment.ohie))),
                            "weights"=ohie.weights[as.numeric(rownames(X.ohie))],
                            "cluster"=ohie.hhid[as.numeric(rownames(X.ohie))])

rct.compliers$complier[rct.compliers$treatment==1 & rct.compliers$insurance==1] <- 1 # true compliers in the treatment group

pred.compliers <- prediction(rct.compliers$C.pscore[rct.compliers$treatment==1], rct.compliers$complier[rct.compliers$treatment==1]) # put in ROCR object # predicted vs. actual compliers

roc.perf <- performance(pred.compliers, measure = "tpr", x.measure = "fpr") # plot ROC curve
plot(roc.perf)
abline(a=0, b= 1)

# Get optimal cut-point
cost.perf <- performance(pred.compliers, "cost")
opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
opt.cut

rct.compliers$complier[rct.compliers$treatment==0 & rct.compliers$C.pscore>opt.cut] <- 1 # predicted compliers from the control group
 
# Fit a regression to the compliers in the RCT
y.col <- seq_along(Y.ohie)

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

load(paste0(repo.directory,"results/response-mod.rda")) 
load(paste0(repo.directory,"results/response-mod-patt.rda")) 

# Print coefficients for Appendix Tables
response.mod$num.visit$cvRisk #A6
response.mod$num.visit$coef 

response.mod$num.out$coef
response.mod$num.out$cvRisk

response.mod$num.visit$coef #A7
response.mod$num.visit$cvRisk

response.mod$num.out$coef
response.mod$num.out$cvRisk

# Use response model to estimate potential outcomes for population "compliers" on medicaid 
nrt.tr.counterfactual <- cbind("insurance" = rep(1, length(which(insurance.nhis==1))),
                               X.nhis[which(insurance.nhis==1),])
nrt.ctrl.counterfactual <- cbind("insurance" = rep(0, length(which(insurance.nhis==1))),
                                 X.nhis[which(insurance.nhis==1),])

Y.hat.1 <- lapply(colnames(Y.ohie), function (i) predict(response.mod[[i]], nrt.tr.counterfactual, onlySL = T)$pred) # extract SL predictions
Y.hat.0 <- lapply(colnames(Y.ohie), function (i) predict(response.mod[[i]], nrt.ctrl.counterfactual, onlySL = T)$pred)

# Compute PATT-C estimator

source(paste0(repo.directory,"code/wtc.R")) # weighted t-test with cluster-bootstrapped SEs

t.patt <- lapply(y.col, function (i) WtC(x=Y.hat.1[[i]], 
                                         y=Y.hat.0[[i]],
                                         bootse=TRUE,
                                         bootp = TRUE,
                                         bootn = 1000,
                                         weight = nhis.weights[which(insurance.nhis == 1)],
                                         weighty=nhis.weights[which(insurance.nhis==1)], 
                                         cluster = nhis.hhid[which(insurance.nhis == 1)],
                                         clustery=nhis.hhid[which(insurance.nhis==1)], 
                                         samedata=FALSE)) 
sanity.check <- TRUE # weighted diff-in-means for sanity check
if(sanity.check){
  lapply(y.col, function (i) weighted.mean(Y.hat.1[[i]], w=nhis.weights[which(insurance.nhis==1)]) - 
           weighted.mean(Y.hat.0[[i]], w=nhis.weights[which(insurance.nhis==1)])) # weight by NHIS survey weights
}

# Compute unadjusted PATT

Y.hat.1.unadj <- lapply(colnames(Y.ohie), function (i) predict(response.mod.patt[[i]], nrt.tr.counterfactual, onlySL = T)$pred)
Y.hat.0.unadj <- lapply(colnames(Y.ohie), function (i) predict(response.mod.patt[[i]], nrt.ctrl.counterfactual, onlySL = T)$pred)

t.patt.unadj <- lapply(y.col, function (i) WtC(x=Y.hat.1.unadj[[i]], 
                                         y=Y.hat.0.unadj[[i]],
                                         bootse=TRUE,
                                         bootp = TRUE,
                                         bootn = 1000,
                                         weight = nhis.weights[which(insurance.nhis == 1)],
                                         weighty=nhis.weights[which(insurance.nhis==1)], 
                                         cluster = nhis.hhid[which(insurance.nhis == 1)],
                                         clustery=nhis.hhid[which(insurance.nhis==1)], 
                                         samedata=FALSE)) 

if(sanity.check){
  lapply(y.col, function (i) weighted.mean(Y.hat.1.unadj[[i]], w=nhis.weights[which(insurance.nhis==1)]) - 
                            weighted.mean(Y.hat.0.unadj[[i]], w=nhis.weights[which(insurance.nhis==1)]))
}

# Compute CACE

rct.cace <- lapply(y.col, function (i) WtC(x=as.matrix(Y.ohie[[i]][which(treatment.ohie==1)]), 
                                               y=as.matrix(Y.ohie[[i]][which(treatment.ohie==0)]), 
                                               c=as.matrix(rct.compliers$complier[which(treatment.ohie==1)]), 
                                               bootse=TRUE,
                                               bootp = TRUE,
                                               bootn = 1000,
                                               weight = ohie.weights[which(treatment.ohie == 1)], 
                                               weighty= ohie.weights[which(treatment.ohie == 0)], 
                                               weightc=rct.compliers$weights[which(treatment.ohie == 1)], 
                                               cluster = ohie.hhid[which(treatment.ohie == 1)], 
                                               clustery=ohie.hhid[which(treatment.ohie==0)], 
                                               clusterc=rct.compliers$cluster[which(treatment.ohie == 1)], 
                                               samedata=FALSE)) 

if(sanity.check){
  lapply(y.col, function (i) (weighted.mean(Y.ohie[[i]][which(treatment.ohie==1)], w=ohie.weights[which(treatment.ohie == 1)]) - # Num. is ITT effect
                                weighted.mean(Y.ohie[[i]][which(treatment.ohie==0)], w=ohie.weights[which(treatment.ohie == 0)]))
         /weighted.mean(rct.compliers$complier[which(treatment.ohie==1)], w=rct.compliers$weights[which(treatment.ohie == 1)])) # Denom. is true RCT compliance rate
}

# Print results for Table A4
t.patt

t.patt.unadj

rct.cace

# Save workspace
save.image(paste0(repo.directory,"data/analysis.RData")) 