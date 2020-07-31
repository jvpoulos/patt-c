# test difference in distributions btwn. outcomes of the observed RCT compliers and the population "compliers"

Y.hat.1.placebo <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.tr.counterfactual, onlySL = T)$pred) # extract SL predictions

# TOST
lapply(y.col, function (i) WtC(Y.ohie[which(insurance.ohie==1),][[i]], 
                                      Y.hat.1.placebo[[i]],
                                      bootse=TRUE,
                                      bootp = FALSE,
                                      bootn = 999,
                                      weight = ohie.weights[which(insurance.ohie == 1)],
                                      weighty=nhis.weights[which(insurance.nhis==1)], 
                                      cluster = ohie.hhid[which(insurance.ohie == 1)],
                                      clustery=nhis.hhid[which(insurance.nhis==1)], 
                                      samedata=FALSE, 
                                      equivalence=TRUE)) 

#T-test 

lapply(y.col, function (i) WtC(Y.ohie[which(insurance.ohie==1),][[i]], 
                               Y.hat.1.placebo[[i]],
                               bootse=TRUE,
                               bootp = FALSE,
                               bootn = 999,
                               weight = ohie.weights[which(insurance.ohie == 1)],
                               weighty=nhis.weights[which(insurance.nhis==1)], 
                               cluster = ohie.hhid[which(insurance.ohie == 1)],
                               clustery=nhis.hhid[which(insurance.nhis==1)], 
                               samedata=FALSE, 
                               equivalence=FALSE)) 

# sanity check (unweighted)
library(equivalence)
lapply(y.col, function (i) tost(Y.ohie[which(insurance.ohie==1),][[i]], 
                               Y.hat.1.placebo[[i]],
                               epsilon = 1, paired = FALSE, var.equal = FALSE)) 