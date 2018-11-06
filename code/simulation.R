#################### Simulation

### Simple design from Freedman (Weighting Regressions by P Scores)
### We assume a treatment effect b which depends on W1: b = 1 if W1 > 0.75, b=-1 if W1 < 0.75
### Y = a + bD + c1W1 + c2W2 + dU is the response model
### Tt = I(e1 + f1W1 + f2W2 + V > 0) is the selection model for treatment when not randomized
### S = I(e2 + g1W1 + g2W2 + g3W3 + R > 0) is the model for selection into the RCT
### C = I(e3 + h2W2 + h3W3 + Q > 0) is the model for compliance
### D depends on C and Tt
### U, V, R, Q are N(0,1); U, V, R, Q, (W1, W2, W3) are mutually independent


rm(list=ls())
library(Matching)
library(MASS)
library(randomForest)
library(rpart)
library(foreach)
library(doParallel)
library(dplyr)
## Setup
ncores <- 12
registerDoParallel(ncores)

sim_estimates <- function(sims = 100, e1= -1, e2 = 0.5, e3 = 1){
  # e1 controls number in the population who are eligible for treatment
  # e2 controls number eligible to be in RCT
  # e3 controls compliance

  # set up storage
  tpatt <- true_patt <- rct_sate <- rct_satt <- tpatt_unadj <- rep(0, sims)
  rateC <- rateT <- rateS <- rep(0, sims)
  
  for(i in 1:sims){
    # Pick target sample size
    popsize <- 30000
    samplesize <- 5000
    rctsample <- sample(1:popsize, samplesize)
    observsample <- (1:popsize)[!(1:popsize %in% rctsample)]
    nrtsample <- sample(observsample, samplesize)
    
    # (U, V, R, Q, W1, W2, W3) are multivariate normal. Set parameters
    a <- c1 <- d <- 1
    c2 <- 2
    f1 <- g2 <- 0.25
    f2 <- g3 <- 0.75
    g1 <- h2 <- h3 <- 0.5
    Sigma <- diag(rep(1,7))
    Sigma[5,5] <- 2
    Sigma[6,6] <- 1
    Sigma[7,7] <- 3
    Sigma[5,6] <- Sigma[6,5] <- 1
    Sigma[5,7] <- Sigma[7,5] <- Sigma[7,6] <- Sigma[6,7] <- 0.5
    mu <- c(0, 0, 0, 0, 0.5, 1, -1)
    # Data for the whole population
    var <- mvrnorm(popsize, mu=mu, Sigma=Sigma, empirical = F)
    U <- var[,1]
    V <- var[,2]
    R <- var[,3]
    Q <- var[,4]
    W1 <- var[,5]
    W2 <- var[,6]
    W3 <- var[,7]
    b <- ifelse(W1 > 0.75, 4, 1)
    Tt = as.numeric((e1 + f1*W1 + f2*W2 + V) > 0 ) # Treatment assigned in NRT
    S = as.numeric(e2 + g1*W1 + g2*W2 + g3*W3 + R > 0) 
    Tt[S == 1] <- sample(c(0,1), sum(S==1), replace = TRUE) # Treatment assigned in RCT
    C <- as.numeric(e3 + h2*W2 + h3*W3 + Q > 0)
    D <- ifelse(C == 1, Tt, 0) # Treatment received
    
    Y <- a + b*D + c1*W1 + c2*W2 + d*U
    dat <- data.frame(Y, Tt, D, S, C, W1, W2, W3)
    rateC[i] <- mean(C)
    rateS[i] <- mean(S)
    rateT[i] <- mean(Tt)
    
    # Set up the RCT
    rct <- dat[rctsample,]
    rct <- rct[rct$S == 1,]
    
    # Set up the non-randomized trial. 
    nrt <- dat[nrtsample,]
    nrt <- nrt[nrt$S==0,]
    nrt <- nrt[,-2]; colnames(nrt)[2] <- "Tt"
    
    # Predict who is a complier in the control group (T=0) using W1, W2, W3
    #  suppressWarnings(complier_mod <- randomForest(C~W1+W2+W3, data = rct)) # estimate propensity of compliance
    complier_mod <- glm(C~W1+W2+W3, data = rct, family = "binomial")
    rct$C_pscore <- predict(complier_mod, rct, type = "response")
    rct$Chat <- rep(0, nrow(rct))
    rct$Chat[rct$Tt == 0] <- as.numeric(rct$C_pscore[rct$Tt == 0] >= 0.5)
    rct_compliers <- rct[rct$Chat == 1 | rct$D == 1, ]
    
    nrt_compliers <- nrt[nrt$Tt == 1,]
    
    # Fit a regression to the compliers in the RCT, use it to predict response in population "compliers"
    response_mod <- randomForest(Y~Tt + W1 + W2 + W3, data = rct_compliers)
    #response_mod <- lm(Y~Tt+W1+W2+W3, data = rct_compliers)
    nrt_tr_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "Tt" = rep(1, nrow(nrt_compliers)))
    nrt_ctrl_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "Tt" = rep(0, nrow(nrt_compliers)))
    nrt_compliers$Yhat_1 <- predict(response_mod, nrt_tr_counterfactual)
    nrt_compliers$Yhat_0 <- predict(response_mod, nrt_ctrl_counterfactual)
    
    
    
    # Compute the estimator
    term1 <- mean(nrt_compliers$Yhat_1[nrt_compliers$Tt==1])
    term2 <- mean(nrt_compliers$Yhat_0[nrt_compliers$Tt==1])
    tpatt[i] <- term1 - term2
    
    # Compare to other estimators
    true_patt[i] <- mean(b[Tt == 1 & S == 0])
    # SATE
    rct_sate[i] <- (mean(rct$Y[rct$Tt == 1]) - mean(rct$Y[rct$Tt==0]))/mean(rct$C[rct$Tt==1])
    # SATT
    term1 <- predict(response_mod, rct_compliers[rct_compliers$D==1,])
    satt_ctrl_counterfactual <- rct_compliers[rct_compliers$D==1,] %>% mutate("Tt" = 0)
    term2 <- predict(response_mod, satt_ctrl_counterfactual)
    rct_satt[i] <- mean(term1) - mean(term2) 
    # Hartman et al estimator - doesn't account for noncompliance
    response_mod2 <- randomForest(Y~Tt + W1 + W2 + W3, data = rct)
    nrt_tr_counterfactual <- cbind(nrt[,c("W1", "W2", "W3")], "Tt" = rep(1, nrow(nrt)))
    nrt_ctrl_counterfactual <- cbind(nrt[,c("W1", "W2", "W3")], "Tt" = rep(0, nrow(nrt)))
    nrt$Yhat_1 <- predict(response_mod2, nrt_tr_counterfactual)
    nrt$Yhat_0 <- predict(response_mod2, nrt_ctrl_counterfactual)
    term1 <- mean(nrt$Yhat_1[nrt$Tt==1])
    term2 <- mean(nrt$Yhat_0[nrt$Tt==1])
    tpatt_unadj[i] <- term1 - term2
    
  }
  res <- cbind(true_patt, tpatt, tpatt_unadj, rct_sate, rct_satt, rateC, rateS, rateT)
  return(res)
}



e <- seq(-2, 2, by = 0.5)
e <- expand.grid(e,e,e)
B <- 10
res <- foreach(i = 1:nrow(e)) %dopar% {
  cat(i)
  return(sim_estimates(B,e[i,1],e[i,2],e[i,3]))
}
res <- do.call(rbind, res)
res <- cbind(rep(1:nrow(e), each = B), res)
colnames(res)[1] <- "combo"
mse <- t(sapply(unique(res[,"combo"]), function(x){
                keep <- which(res[,"combo"] == x)
                sapply(c("tpatt","tpatt_unadj","rct_sate","rct_satt"), function(cc)mean((res[keep,"true_patt"]-res[keep,cc])^2))
                }))
mse_dup <- matrix(NA, ncol = 4, nrow = B*nrow(e))
colnames(mse_dup) <- c("mse_tpatt", "mse_tpatt_unadj", "mse_rct_sate", "mse_rct_satt")
for(i in 1:4){mse_dup[,i] <- rep(mse[,i], each = B)}
res <- cbind(res, mse_dup)
res <- as.data.frame(res)
save(res, file = "simulation_res.Rdata")
