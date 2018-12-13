# test difference in distributions btwn. outcomes of the observed RCT compliers and the population "compliers"

Y.hat.1.placebo <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], return(predict(response.mod.binary[[i]], nrt.tr.counterfactual, onlySL = T)$pred),
                                                        return(predict(response.mod.num[[i]], nrt.tr.counterfactual, onlySL = T)$pred))) # extract SL predictions

# Any er. visits
t.test(Y.ohie[which(insurance.ohie==1),][[1]], Y.hat.1.placebo[[1]]) # two sided

mean(Y.ohie[which(insurance.ohie==1),][[1]]) - mean(Y.hat.1.placebo[[1]])
# Num er. visits
t.test(Y.ohie[which(insurance.ohie==1),][[2]], Y.hat.1.placebo[[2]]) # two sided

mean(Y.ohie[which(insurance.ohie==1),][[2]]) - mean(Y.hat.1.placebo[[2]])

# num outpatient visits
t.test(Y.ohie[which(insurance.ohie==1),][[4]], Y.hat.1.placebo[[4]]) # two sided
mean(Y.ohie[which(insurance.ohie==1),][[4]]) - mean(Y.hat.1.placebo[[4]])