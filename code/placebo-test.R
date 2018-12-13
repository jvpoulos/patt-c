# test difference in distributions btwn. outcomes of the adjusted RCT compliers and the group who took up treatment in the population

# rct.tr.counterfactual.adj <- cbind("insurance" = rep(1, length(which(insurance.ohie==1))),
#                                      X.ohie[which(insurance.ohie==1),])
# 
# Y.ohie[which(insurance.ohie==1),] <- lapply(colnames(Y.ohie), function (i) ifelse(i%in%colnames(Y.ohie)[y.col.binary], return(predict(response.mod.binary[[i]], rct.tr.counterfactual.adj, onlySL = T)$library.predict[,10]),
#                                                                 return(predict(response.mod.num[[i]], rct.tr.counterfactual.adj, onlySL = T)$pred))) 
# 
#Y.hat.1# adjusted pop. compliers 

#Y.ohie[which(insurance.ohie==1),] # RCT insured

# Any er. visits
mean(Y.ohie[which(insurance.ohie==1),][[1]])
mean(Y.hat.1[,1])

wilcox.test(Y.ohie[which(insurance.ohie==1),][[1]], Y.hat.1[[1]]) # two sided

# Num er. visits
wilcox.test(Y.ohie[which(insurance.ohie==1),][[2]], Y.hat.1[[2]]) # two sided

# num outpatient visits
wilcox.test(Y.ohie[which(insurance.ohie==1),][[4]], Y.hat.1[[4]]) # two sided