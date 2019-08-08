library(weights)

# test difference in distributions btwn. outcomes of the observed RCT compliers and the population "compliers"

Y.hat.1.placebo <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], return(predict(response.mod.binary[[i]], nrt.tr.counterfactual, onlySL = T)$pred),
                                                        return(predict(response.mod.num[[i]], nrt.tr.counterfactual, onlySL = T)$pred))) # extract SL predictions

# Num ER visits
wtd.t.test(Y.ohie[which(insurance.ohie==1),][[1]], Y.hat.1.placebo[[1]], 
           weightx = w=ohie.weights[which(treatment.ohie == 1)], 
           weighty=nhis.weights[which(insurance.nhis==1)], samedata=FALSE) # two sided

weighted.mean(Y.ohie[which(insurance.ohie==1),][[1]]) - weighted.mean(Y.hat.1.placebo[[1]])

# num outpatient visits
wtd.t.test(Y.ohie[which(insurance.ohie==1),][[2]], Y.hat.1.placebo[[2]],
           weightx = w=ohie.weights[which(treatment.ohie == 1)], 
           weighty=nhis.weights[which(insurance.nhis==1)], samedata=FALSE) # two sided

weighted.mean(Y.ohie[which(insurance.ohie==1),][[2]]) - weighted.mean(Y.hat.1.placebo[[2]])