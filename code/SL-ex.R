run <- FALSE

# Example for binary classification
if(run){
  set.seed(42)
  fitSL <- SuperLearner(Y=Y,X=X,
                        SL.library=SL.library.class,
                        family=binomial()) # glmnet response is 2-level factor
  
  fitSL # summarize
}

# Example for regression
if(run){
  # Get risk estimates with 10-fold CV
  set.seed(42)
  fitSL.CV <- CV.SuperLearner(Y=y07[,1],X=x07,
                              SL.library=SL.library.reg,
                              family=gaussian(),
                              cvControl =list(V=10L)) 
  summary(fitSL.CV)
  latex(summary(fitSL.CV))  
  
  # Fit model
  fitSL <- SuperLearner(Y=Y,X=X,
                        SL.library=SL.library.reg,
                        family=gaussian()) 
  
  fitSL # summarize
}
