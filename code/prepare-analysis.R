## Imports NHIS and OHIE datasets and creates outcome vectors and common covariates for the analysis 

# Libraries
library(weights)
library(plyr)

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source data prep scripts
suppressWarnings(source(file.path(directory,"prepare-ohie.R")))
source(file.path(directory,"prepare-nhis.R")) # script merges person, sample adult, and imputed income files

## NHIS: sample selection

# Keep participants below 138% FPL
nhis[["2008"]] <- subset(nhis[["2008"]], nhis[["2008"]]$rat_cati <= 4) # < 124%
nhis[["2009"]] <- subset(nhis[["2009"]], nhis[["2009"]]$povrati2 <= 138) # diff'nt var
nhis[["2010"]] <- subset(nhis[["2010"]], nhis[["2010"]]$povrati3 <= 1380) 
nhis[["2011"]] <- subset(nhis[["2011"]], nhis[["2011"]]$povrati3 <= 1380)
nhis[["2012"]] <- subset(nhis[["2012"]], nhis[["2012"]]$povrati3 <= 1.380) # diffn't decimals
nhis[["2013"]] <- subset(nhis[["2013"]], nhis[["2013"]]$povrati3 <= 1.380)

# Age 19-64
foreach(i=years) %do% {
  nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], nhis[[as.character(i)]]$age_p>=19 &
                                      nhis[[as.character(i)]]$age_p<=64)
}

# With medicaid or uninsured
foreach(i=years) %do% {
  nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], 
                                    nhis[[as.character(i)]]$medicaid==1 | nhis[[as.character(i)]]$medicaid==2  | # with medicaid
                                      nhis[[as.character(i)]]$notcov==1) # not covered
}

## OHIE: create vectors for treatment, # of HH members, and compliance status

# Treatment assignment
treatment <- ifelse(ohie$treatment=="Selected",1,0)

# Assignment is random only conditional on # of HH members on waiting list 
n.hh <- dummify(ohie$numhh_list,keep.na=TRUE)

# Compliance is "ever on Medicaid" during study period (used in Finkelstein et al. (2012))
insurance <- ifelse(ohie$ohp_all_ever_matchn_30sep2009=="Enrolled",1,0) 

addmargins(table(insurance, treatment)) # there's two-way crossover?

## NHIS: compliance analogue

# Medicaid recode  
medicaid <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$medicaid[nhis[[as.character(i)]]$medicaid>3] <- NA # Medicaid recode: missing is NA
  ifelse(nhis[[as.character(i)]]$medicaid==1 | nhis[[as.character(i)]]$medicaid==2,1,0)
}

## OHIE: create vectors for health care use outcomes  (used in Finkelstein et al. (2012))
# (Twelve Month Mail Survey)

# Any ER visit 
any.visit <- NA
any.visit[ohie$er_any_12m=="Yes"] <- 1
any.visit[ohie$er_any_12m=="No"] <- 0

# Num of ER visits, truncated at 2*99th%ile
num.visit <- ohie$er_num_mod_12m

# Any hospital visits
any.hosp <- NA
any.hosp[ohie$hosp_any_12m=="Yes"] <- 1
any.hosp[ohie$hosp_any_12m=="No"] <- 0

# Number hospital visits, truncated at 2*99th%ile
num.hosp <- ohie$hosp_num_mod_12m

# Any primary care (outpatient) visits
any.out <- NA
any.out[ohie$doc_any_12m=="Yes"] <- 1
any.out[ohie$doc_any_12m=="No"] <- 0

# Num prim. care visits,truncated at 2*99th%ile
num.out <- ohie$doc_num_mod_12m

## Create NHIS outcome vectors

# Number of times in ER/ED, past 12 m
nhis.num.visit <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$ahernoy2
}
nhis.num.visit[nhis.num.visit==97 | nhis.num.visit==98 | nhis.num.visit==99]  <- NA # make missing NA

# Any ER/ED visit in past 12 m
nhis.any.visit <- NA
nhis.any.visit[nhis.num.visit==0] <- 0
nhis.any.visit[nhis.num.visit>0] <- 1

# Total number of office visits, past 12 m 
nhis.num.out <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$ahcnoyr
}
nhis.num.out[nhis.num.out==97 | nhis.num.out==98 | nhis.num.out==99]  <- NA # make missing NA

# Any office visit in last 12 m
nhis.any.out <- NA
nhis.any.out[nhis.num.out==0] <- 0
nhis.any.out[nhis.num.out>0] <- 1

## Create vectors for common covariates
## Note: OHIE variables are pretreatment (Initial Mail Survey dataset)

# No. people in HH
n.hh.nhis <- foreach(i=years, .combine=c) %do% {  
  cut(nhis[[as.character(i)]]$acpt_per, 
      breaks=c(-Inf,1,2,Inf))
}
n.hh.nhis <- factor(n.hh.nhis)
levels(n.hh.nhis) <- colnames(n.hh)
n.hh.nhis <- dummify(n.hh.nhis, keep.na=TRUE)

# Gender
gender <- dummify(ohie$female_0m,keep.na=TRUE)
gender.nhis <- foreach(i=years, .combine=rbind) %do% {
  dummify(factor(nhis[[as.character(i)]]$sex),keep.na=TRUE)
}
colnames(gender.nhis) <- c("Male","Female")

# Age 19–49
age.19to49 <- ifelse(ohie$birthyear_0m<1958,1,0)
age.19to49.nhis <- foreach(i=years, .combine=c) %do% {
   ifelse(nhis[[as.character(i)]]$age_p<=49,1,0)
}

# Age 50–64
age.50to64 <- ifelse(ohie$birthyear_0m>=1958,1,0)
age.50to64.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$age_p>=50,1,0)
}

# Race: white
white <- ifelse(ohie$race_white_0m=="Yes",1,0)
white.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$racerpi2==1,1,0)
}

# Race: black
black <- ifelse(ohie$race_black_0m=="Yes",1,0)
black.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$racerpi2==2,1,0)
}

# Ethnicity: Spanish/Hispanic/Latino
hisp <- ifelse(ohie$race_hisp_0m=="Yes",1,0)
hisp.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$origin_i==1,1,0)
}

# Diagnosed with diabetes
diabetes <- ifelse(ohie$dia_dx_0m=="Diagnosed",1,0)
diabetes.nhis <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$dibev[nhis[[as.character(i)]]$dibev>2] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$dibev==1,1,0)
}

# Diagnosed with asthma
asthma <- ifelse(ohie$ast_dx_0m=="Diagnosed",1,0)
asthma.nhis <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$aasmev[nhis[[as.character(i)]]$aasmev>2] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$aasmev==1,1,0)
}

# Diagnosed with high blood pressure
bp <- ifelse(ohie$hbp_dx_0m=="Diagnosed",1,0)
bp.nhis <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$hypev[nhis[[as.character(i)]]$hypev>2] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$hypev==1,1,0)
}

# Diagnosed with heart condition/disease
heart <- ifelse(ohie$chf_dx_0m=="Diagnosed",1,0)
heart.nhis <- foreach(i=years, .combine=c) %do% {  
  nhis[[as.character(i)]]$hrtev[nhis[[as.character(i)]]$hrtev>2] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$hrtev==1,1,0)
}

# Highest level of education completed
education <- dummify(ohie$edu_0m,keep.na=TRUE)
education.nhis <- foreach(i=years, .combine=c) %do% {  
  nhis[[as.character(i)]]$educ[nhis[[as.character(i)]]$educ>21] <- NA # missing is NA
  cut(nhis[[as.character(i)]]$educ, 
      breaks=c(-Inf,12,14,17,Inf)) 
}
education.nhis <- factor(education.nhis)
levels(education.nhis) <- colnames(education)
education.nhis <- dummify(education.nhis, keep.na=TRUE)

# HH income level (three groups)
income3 <- cut(as.numeric(ohie$hhinc_cat_0m),  #OHIE
               breaks=c(-Inf,5,11,Inf),
               labels=c("$0-$10000","$10001-$25000",">$25000")) 
income <- dummify(income3,keep.na=TRUE)

income.nhis <- foreach(i=years, .combine=c) %do% { # NHIS
  if(i %in% c(2009:2013)){
  cut(nhis[[as.character(i)]]$faminci2, 
      breaks=c(-Inf,10000,25000,Inf),
      labels=c("$0-$10000","$10001-$25000",">$25000")) 
  } else{
    cut(nhis[[as.character(i)]]$incgrpi2,
        breaks=c(-Inf,2,5,Inf),
        labels=c("$0-$10000","$10001-$25000",">$25000")) 
  }
}

income.nhis <- factor(income.nhis)
levels(income.nhis) <- colnames(income)
income.nhis <- dummify(income.nhis, keep.na=TRUE)

