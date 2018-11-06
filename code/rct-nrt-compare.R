library(reporttools)

# Create table similar to Table 1 of Hartman et al. 
rct.nrt.tab <- rbind(cbind(study="OHIE",X.ohie.response,
                           Y.ohie.response[c("any.visit","any.out")]),
                     cbind(study="NHIS",nrt.tr.counterfactual,
                           Y.nhis[c("any.visit","any.out")][which(insurance.nhis==1),])) # create data for table
rct.nrt.tab$group <- NA
rct.nrt.tab$group[rct.nrt.tab$study=="OHIE" & rct.nrt.tab$treatment==0] <- 1
rct.nrt.tab$group[rct.nrt.tab$study=="OHIE" & rct.nrt.tab$treatment==1] <- 2
rct.nrt.tab$group[rct.nrt.tab$study=="NHIS" & rct.nrt.tab$treatment==1] <- 3

tableNominal(vars = rct.nrt.tab, 
             group = rct.nrt.tab$group, 
             #      nams=c(cov.names[-1],"Any ER visit","Any primary care visit"),
             vertical=FALSE,
             prec = 3,cumsum=FALSE,lab = "rct-nrt-compare",
             cap="Pretreatment covariates and responses for the OHIE and for NHIS respondents who received Medicaid.") # RCT vs. NRT compliers


