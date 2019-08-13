library(reporttools)

# Table A1 
rct.nrt.tab <- rbind(cbind(study="OHIE",
                           X.ohie.response,
                           Y.ohie.response[c("num.visit","num.out")],
                           "survey.weights"=ohie.weights),
                     cbind(study="NHIS",
                           X.nhis[which(insurance.nhis==1),],
                           Y.nhis[c("num.visit","num.out")][which(insurance.nhis==1),],
                           "survey.weights"=nhis.weights[which(insurance.nhis==1)])) # create data for table
rct.nrt.tab$group <- NA
rct.nrt.tab$group[rct.nrt.tab$study=="OHIE" & rct.nrt.tab$insurance==0] <- 1
rct.nrt.tab$group[rct.nrt.tab$study=="OHIE" & rct.nrt.tab$insurance==1] <- 2
rct.nrt.tab$group[rct.nrt.tab$study=="NHIS" & rct.nrt.tab$insurance==1] <- 3

tableContinuous(vars = rct.nrt.tab[colnames(rct.nrt.tab)%in%c("num.visit", "num.out")], 
             weights = rct.nrt.tab$survey.weights, # survey weights
             group = rct.nrt.tab$group, 
             prec = 3,
             cumsum=FALSE) # RCT vs. NRT compliers

# Table A2 

tableContinuous(vars = rct.compliers$treatment, 
                weights = rct.compliers$weights, # survey weights
                group = rct.compliers$insurance, 
                stats= "n",
                prec = 0,
                cumsum=TRUE)