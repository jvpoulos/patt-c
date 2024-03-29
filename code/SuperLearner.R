library(SuperLearner)
library(class)
library(randomForest)
library(glmnet)
library(gam)
library(e1071)
library(gbm)
library(xgboost)

# Creates additional randomForest wrappers changing both mtry and nodesize
tuneGrid <- expand.grid(mtry=c(1,5,10), nodesize=c(1,5))
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10), nodesize = c(1, 5))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for(mm in seq(nrow(tuneGrid))) { 
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest()

# Creates knn wrappers in the global environment with different nearest neighbors. The default value for k in SL.knn is 10
create.SL.knn <- function(k = c(20, 30, 40, 50)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm], ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.knn()

# Creates glmnet wrappers in the global environment with different alpha. The default value for alpha in SL.glmnet is 1
create.SL.glmnet <- function(alpha = c(0,0.25, 0.50, 0.75)) {
  for(mm in seq(length(alpha))){
    eval(parse(text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet()


# creates gam wrappers in the global environment with different degrees. The default value for deg.gam in SL.gam is 2
create.SL.gam <- function(deg.gam = c(3, 4)) {
  for(mm in seq(length(deg.gam))){
    eval(parse(text = paste('SL.gam.', deg.gam[mm], '<- function(..., deg.gam = ', deg.gam[mm], ') SL.gam(..., deg.gam = deg.gam)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.gam()

# creates gbm wrappers in the global environment with different distributions (1=bernoulli (logistic reg.), 2=AdaBoost exponential loss)
create.SL.gbm <- function(distribution = c("bernoulli","adaboost","gaussian")) {
  for(mm in seq(length(distribution))){
    eval(parse(text = paste('SL.gbm.', distribution[mm], '<- function(..., distribution = ', distribution[mm], ') SL.gbm(..., distribution = distribution)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.gbm()


SL.mean <- function (Y, X, newX, family, obsWeights, id, ...) 
{
  meanY <- weighted.mean(Y, w = obsWeights)
  pred <- rep.int(meanY, times = nrow(newX))
  fit <- list(object = meanY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mean")
  return(out)
}

predict.SL.mean <- function (object, newdata, family, X = NULL, Y = NULL, ...) 
{
  pred <- rep.int(object$object, times = nrow(newdata))
  return(pred)
}

# Define library
SL.library.class<- c("SL.gbm.adaboost",
                     "SL.gbm.bernoulli",
		    "SL.glmnet", # lasso
		    "SL.glmnet.0.25",
		    "SL.glmnet.0.5",
		    "SL.glmnet.0.75",
		    "SL.glmnet.0", # ridge
		                "SL.xgboost",
                    "SL.randomForest.1",
		                 "SL.randomForest.3")# nodesize=1 for regression

SL.library.reg <- c("SL.gam", # degree=2
                     "SL.gam.3",
                     "SL.gam.4",
                     "SL.gbm.gaussian",
                     "SL.glmnet", # lasso
                     "SL.glmnet.0", # ridge
                     "SL.glmnet.0.25",
                     "SL.glmnet.0.5",
                     "SL.glmnet.0.75",
                     "SL.randomForest.4", # nodesize=5 for regression
                     "SL.randomForest.6")