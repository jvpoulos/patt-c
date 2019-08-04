## Imports NHIS and OHIE datasets and creates outcome vectors and common covariates for the analysis 

# Libraries # need plyr
library(weights) # install cluster -> HMisc -> weights
library(foreach)

years <- c(2008:2017)

# Import RCT and population data
ohie <- readRDS(paste0(repo.directory,"data/prepare-ohie.RData"))
nhis <- readRDS(paste0(repo.directory,"data/prepare-NHIS.RData")) 

## NHIS: sample selection

# Keep participants below 138% FPL
nhis[["2008"]] <- subset(nhis[["2008"]], nhis[["2008"]]$rat_cati <= 4) # < 124%
nhis[["2009"]] <- subset(nhis[["2009"]], nhis[["2009"]]$povrati2 <= 138) # diff'nt var

invisible(foreach(i=c(2010,2011)) %do% {
nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], nhis[[as.character(i)]]$povrati3 <= 1380) 
nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], nhis[[as.character(i)]]$povrati3 <= 1380)
})

invisible(foreach(i=c(2012:2017)) %do% {
nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], nhis[[as.character(i)]]$povrati3 <= 1.380) # diffn't decimals
nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], nhis[[as.character(i)]]$povrati3 <= 1.380)
})

# Age 19-64
invisible(foreach(i=years) %do% {
  nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], nhis[[as.character(i)]]$age_p>=19 &
                                      nhis[[as.character(i)]]$age_p<=64)
})

# With medicaid or uninsured
# Medicaid - Includes persons who do not have private coverage, but who have Medicaid or other state sponsored health plans including CHIP.
invisible(foreach(i=years) %do% {
  nhis[[as.character(i)]] <- subset(nhis[[as.character(i)]], 
                                    nhis[[as.character(i)]]$medicaid==1 | nhis[[as.character(i)]]$medicaid==2  | # with medicaid
                                      nhis[[as.character(i)]]$notcov==1) # not covered
})

## OHIE: create vectors for treatment, # of HH members, compliance status, household ID, survey weights, and survey wave

# Treatment assignment
treatment <- ifelse(ohie$treatment=="Selected",1,0)

# Assignment is random only conditional on # of HH members on waiting list 
n.hh <- dummify(ohie$numhh_list,keep.na=TRUE)

# Compliance is "ever on Medicaid" during study period (used in Finkelstein et al. (2012))
insurance <- ifelse(ohie$ohp_all_ever_matchn_30sep2009=="Enrolled",1,0) 

addmargins(table(insurance, treatment)) # there's two-way crossover?

# Household ID
ohie.hhid <- ohie$household_id

# 12 month survey weights
ohie.weights <- ohie$weight_12m

# 12 month survey wave
wave <- dummify(ohie$wave_survey12m,keep.na=TRUE)

# Wave X hh size interaction

wave.interact <- foreach(i=seq_len(ncol(n.hh)), .combine='cbind') %do% {
  wave*n.hh[,i]
}

## NHIS: compliance analogue, household ID, survey weights, and survey wave

# Medicaid recode  
medicaid <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$medicaid[nhis[[as.character(i)]]$medicaid>3] <- NA # Medicaid recode: missing is NA
  ifelse(nhis[[as.character(i)]]$medicaid==1 | nhis[[as.character(i)]]$medicaid==2,1,0)
}

# household ID
nhis.hh.id <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$hhx
}

# survey weights
nhis.weights <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$wtfa_hh
}

# nhis wave (matrix of 1s)
wave.nhis <- matrix(1,nrow=length(nhis.weights), ncol=ncol(wave))

wave.nhis.interact <- foreach(i=seq_len(ncol(n.hh.nhis)), .combine='cbind') %do% {
  wave.nhis*n.hh.nhis[,i]
}

## OHIE: create vectors for health care use outcomes  (used in Finkelstein et al. (2012))
# (Twelve Month Mail Survey)

# Num of ER visits, past 6 m, truncated at 2*99th%ile
num.visit <- ohie$er_num_mod_12m

# Num prim. care visits, past 6 m, truncated at 2*99th%ile
num.out <- ohie$doc_num_mod_12m

## Create NHIS outcome vectors 

# Number of times in ER/ED, past 12 m
nhis.num.visit <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$ahernoy2
}
nhis.num.visit[nhis.num.visit==97 | nhis.num.visit==98 | nhis.num.visit==99]  <- NA # make missing NA

nhis.num.visit[!is.na(nhis.num.visit)] <- nhis.num.visit[!is.na(nhis.num.visit)]/2 # halve non-missing visits

# Total number of office visits, past 12 m 
nhis.num.out <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$ahcnoyr
}
nhis.num.out[nhis.num.out==97 | nhis.num.out==98 | nhis.num.out==99]  <- NA # make missing NA

nhis.num.out[!is.na(nhis.num.out)] <- nhis.num.out[!is.na(nhis.num.out)]/2 # halve non-missing outs

## Create vectors for common covariates
## Note: OHIE variables are pretreatment (Initial Mail Survey dataset)

# No. children in HH
n.children <- cut(ohie$num19_0m, breaks=c(-Inf,0,2,Inf))
n.children  <- factor(n.children)
n.children  <- dummify(n.children, keep.na=TRUE)

n.children.nhis <- foreach(i=years, .combine=c) %do% {  
  cut(nhis[[as.character(i)]]$acptchld,
      breaks=c(-Inf,0,2,Inf))
}
n.children.nhis <- factor(n.children.nhis) 
levels(n.children.nhis) <- colnames(n.children)
n.children.nhis <- dummify(n.children.nhis, keep.na=TRUE)

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

# Race: asian
asian <- ifelse(ohie$race_asian_0m=="Yes",1,0)
asian.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$racerpi2==4,1,0)
}

# Race: American Indian or Alaska Native
aian <- ifelse(ohie$race_amerindian_0m=="Yes",1,0)
aian.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$racerpi2==3,1,0)
}

# Race: Other
race.other <- ifelse(ohie$race_other_qn_0m=="Yes",1,0)

race.other.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$racerpi2%in%c(3,6),1,0)
}

# Race: Spanish/Hispanic/Latino
hisp <- ifelse(ohie$race_hisp_0m=="Yes",1,0)
hisp.nhis <- foreach(i=years, .combine=c) %do% {
  ifelse(nhis[[as.character(i)]]$origin_i==1,1,0)
}

# Diagnosed with Diabetes or Sugar Diabetes
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

# Diagnosed with Emphysema or Chronic Bronchitis (COPD)
copd <- ifelse(ohie$emp_dx_0m=="Diagnosed",1,0)
copd.nhis <- foreach(i=years, .combine=c) %do% {
  if(i%in%c(2008:2011)){
  nhis[[as.character(i)]]$ephev[nhis[[as.character(i)]]$ephev>2] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$ephev==1,1,0)
  }else{
    nhis[[as.character(i)]]$copdev[nhis[[as.character(i)]]$copdev>2] <- NA # missing is NA
    ifelse(nhis[[as.character(i)]]$copdev==1,1,0)
  }
}

# Diagnosed with Congestive Heart Failure
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
  if(i %in% c(2009:2017)){
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

# Living with Partner/Spouse
partner <- ifelse(ohie$living_arrange_0m=="Live with partner/spouse",1,0)
partner.nhis <- foreach(i=years, .combine=c) %do% {  
  nhis[[as.character(i)]]$r_maritl[nhis[[as.character(i)]]$r_maritl==9] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$r_maritl%in%c(1,8),1,0)
}

#  Currently employed or self-employed
employed <- ifelse(ohie$employ_0m=="Yes",1,0)
employed.nhis <- foreach(i=years, .combine=c) %do% {  
  nhis[[as.character(i)]]$doinglwp[nhis[[as.character(i)]]$doinglwp>=7] <- NA # missing is NA
  ifelse(nhis[[as.character(i)]]$doinglwp%in%c(1,2,4),1,0)
}

# save data
save.image(paste0(repo.directory,"data/prepare-analysis.RData"))