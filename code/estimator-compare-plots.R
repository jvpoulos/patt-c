# Create plots for each outcome variable comparing SATT vs. PATT overall and by covariate 

# Load R workspace
load(paste0(repo.directory,"data/analysis.RData"))

# Libraries
library(ggplot2)

### Confidence intervals for the estimates
B <- 1000

# PATT-C
t.patt.boot <- replicate(B, {
  samp <- sample(1:length(Y.hat.1[[1]]), length(Y.hat.1[[1]]), replace=T)  
  lapply(y.col, function (i) weighted.mean(Y.hat.1[[i]][samp], w=nhis.weights[which(insurance.nhis[samp]==1)]) -
           weighted.mean(Y.hat.0[[i]][samp], w=nhis.weights[which(insurance.nhis[samp]==1)]))
})
t.patt.ci <- lapply(y.col, function(i) quantile(unlist(t.patt.boot[i,]),probs = c(0.025, 0.975)))

# PATT
t.patt.unadj.boot <- replicate(B, {
  samp <- sample(1:length(Y.hat.1.unadj[[1]]), length(Y.hat.1.unadj[[1]]), replace=T)  
  lapply(y.col, function (i) weighted.mean(Y.hat.1.unadj[[i]][samp], w=nhis.weights[which(insurance.nhis[samp]==1)]) - 
           weighted.mean(Y.hat.0.unadj[[i]][samp]), w=nhis.weights[which(insurance.nhis[samp]==1)])
})
t.patt.unadj.ci <- lapply(y.col, function(i) quantile(unlist(t.patt.unadj.boot[i,]),probs = c(0.025, 0.975)))

# CACE
t.cace.boot <- replicate(B,{
  samp <- sample(1:length(treatment.ohie), length(treatment.ohie), replace=T)  
  lapply(y.col, function (i) (weighted.mean(Y.ohie[[i]][samp][which(treatment.ohie[samp]==1)], w=ohie.weights[which(treatment.ohie[samp] == 1)]) - # Num. is ITT effect
                                weighted.mean(Y.ohie[[i]][samp][which(treatment.ohie[samp]==0)], w=ohie.weights[which(treatment.ohie[samp] == 0)])) 
         /weighted.mean(rct.compliers$complier[samp][which(treatment.ohie[samp]==1)], w=rct.compliers$weights[samp][which(treatment.ohie == 1)]))
})
t.cace.ci <- lapply(y.col, function(i) quantile(unlist(t.cace.boot[i,]),probs = c(0.025, 0.975)))

# print results for table 
t.patt
t.patt.ci

t.patt.unadj
t.patt.unadj.ci

rct.cace
t.cace.ci

### Function to get heterogeneous treatment effect estimates, using true data and bootstrapped data (set boot = TRUE)

het.effects <- function(covs, boot = FALSE){
  if(boot==TRUE){
    boot.nrt <- sample(1:sum(insurance.nhis==1,na.rm=T), sum(insurance.nhis==1,na.rm=T), replace = T)
    Y.hat.1 <- lapply(y.col, function(i) Y.hat.1[[i]][boot.nrt])
    Y.hat.0 <- lapply(y.col, function(i) Y.hat.0[[i]][boot.nrt])
    X.nhis_boot <- X.nhis[which(insurance.nhis==1),][boot.nrt,]
    
    boot.nrt.unadj <- sample(1:sum(insurance.nhis==1 | insurance.nhis==0,na.rm=T), sum(insurance.nhis==1 | insurance.nhis==0,na.rm=T), replace = T)
    Y.hat.1.unadj <- lapply(y.col, function(i) Y.hat.1.unadj[[i]][boot.nrt.unadj])
    Y.hat.0.unadj <- lapply(y.col, function(i) Y.hat.0.unadj[[i]][boot.nrt.unadj])
    X.nhis_unadjboot <- X.nhis[which(insurance.nhis==1 | insurance.nhis == 0),][boot.nrt.unadj,]
    
    boot.rct <- sample(1:nrow(X.ohie), nrow(X.ohie), replace = T)
    treatment.ohie <- treatment.ohie[boot.rct]
    insurance.ohie <- insurance.ohie[boot.rct]
    Y.ohie <- Y.ohie[boot.rct,]
    X.ohie <- X.ohie[boot.rct,]
    rct.compliers <- rct.compliers[boot.rct,]
    X.ohie.response <- data.frame("treatment"=treatment.ohie[which(rct.compliers$complier==1)],
                                  X.ohie[which(rct.compliers$complier==1),])
  }else{
    X.nhis_boot      <- X.nhis[which(insurance.nhis==1),]
    X.nhis_unadjboot <- X.nhis[which(insurance.nhis==1 | insurance.nhis==0),]
  }
  
  # Calculate differences in potential outcomes for population treated compliers
  nrt.pred <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1[[i]]-Y.hat.0[[i]], 
                                                    X.nhis_boot))
  
  # Estimate PATT for each covariate group
  
  patt.het <- lapply(y.col, function (i) lapply(covs, function(x) weighted.mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==1]) - 
                                                  weighted.mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==0]))) # heterogenous treatment effect on population treated compliers
  
  # For comparison, calculate differences in potential outcomes for population treated 
  nrt.pred.unadj <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1.unadj[[i]]-Y.hat.0.unadj[[i]],
                                                          X.nhis_unadjboot))
  
  # Estimate unadjusted PATT for each covariate group
  patt.unadj.het <- lapply(y.col, function (i) lapply(covs, function(x) weighted.mean(nrt.pred.unadj[[i]]$tau[nrt.pred.unadj[[i]][x]==1]) - 
                                                        weighted.mean(nrt.pred.unadj[[i]]$tau[nrt.pred.unadj[[i]][x]==0])))  
  
  # Estimate CACE for each covariate group
  cace.het <- lapply(y.col, function (i) lapply(covs, function(x) (weighted.mean(Y.ohie[[i]][which(treatment.ohie==1)][X.ohie.response[x]==1]) - 
                                                                     weighted.mean(Y.ohie[[i]][which(treatment.ohie==0)][X.ohie.response[x]==1])) 
                                                /weighted.mean(rct.compliers$complier[which(treatment.ohie==1)][X.ohie.response[x]==1]))) # heterogenous treatment effect on sample treated compliers
  
  return(list(patt.het, patt.unadj.het, cace.het))
}


### Estimate the heterogeneous effects
covs <- colnames(X.nhis)[5:21] # choose features to est. het effects

true_effect <- het.effects(covs) # a list where true_effect[[1]] is patt.het, true_effect[[2]] is patt.unadj.het, etc
patt.het <- true_effect[[1]]
patt.unadj.het <- true_effect[[2]]
cace.het <- true_effect[[3]]

B <- 1000
boot_effect <- replicate(B, het.effects(covs,boot=TRUE))

### Compute the 95% confidence intervals based on the quantiles of the bootstrap distribution

patt.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[1,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
patt.unadj.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[2,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
cace.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[3,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
conf.int <- lapply(y.col, function(i){
  ci.lower <- c(t.patt.ci[[i]][1], sapply(patt.het.boot.ci, "[[", i)[1,],  #### Put in 0 in place of "overall" confidence bounds for now
                t.patt.unadj.ci[[i]][1], sapply(patt.unadj.het.boot.ci, "[[", i)[1,],
                t.cace.ci[[i]][1], sapply(cace.het.boot.ci, "[[", i)[1,])
  ci.upper <- c(t.patt.ci[[i]][2], sapply(patt.het.boot.ci, "[[", i)[2,],
                t.patt.unadj.ci[[i]][2], sapply(patt.unadj.het.boot.ci, "[[", i)[2,],
                t.cace.ci[[i]][2], sapply(cace.het.boot.ci, "[[", i)[2,])
  cbind(ci.lower, ci.upper)
})

# Find middle of bootstrap CIs for cace

cace.het.m <- lapply(y.col, function (i) (sapply(cace.het.boot.ci, "[[", i)[1,]+sapply(cace.het.boot.ci, "[[", i)[2,])/2)

# Create data for plot
Overall  <- c("Overall")
Sex <- c("Female")
Age <- c("19-49", "50-64")
Race.ethn <- c("White", "Black", "Hispanic")
Health.stat <- c( "Diabetes", "Asthma","HBP", "Heart Condition")
Education <- c("< HS","HS/GED",
               "Voc./2-year deg.","4-year deg.")
Income <- c("< $10k","$10k-$25k","> $25k")
cov.groups <- c("Overall","Sex","Age","Race","Health status","Education","Income") 
cov.names <- c(Overall,Sex,Age,Race.ethn,Health.stat,Education,Income)

het.plot <- lapply(y.col, function (i) data.frame(x=factor(c(rep(cov.names,3)), levels=rev(cov.names)), 
                                                  y = c(t.patt[[i]],unlist(patt.het[[i]]),
                                                        t.patt.unadj[[i]],unlist(patt.unadj.het[[i]]),
                                                        rct.cace[[i]],unlist(cace.het.m[i])), 
                                                  Group = factor(rep(c(cov.groups[1],rep(cov.groups[2],length(Sex)),rep(cov.groups[3],length(Age)),
                                                                       rep(cov.groups[4],length(Race.ethn)),rep(cov.groups[5],length(Health.stat)),
                                                                       rep(cov.groups[6],length(Education)),rep(cov.groups[7],length(Income))),3), levels=cov.groups),
                                                  Estimator= factor(c(rep("PATT-C",length(covs)+1),
                                                                      rep("PATT",length(covs)+1),
                                                                      rep("CACE",length(covs)+1))), 
                                                  ci.lower = conf.int[[i]][,1],
                                                  ci.upper = conf.int[[i]][,2]))
for(i in y.col){
  het.plot[[i]] <- subset( het.plot[[i]], Estimator!="CACE") #rm cace
  het.plot[[i]]$Estimator <- factor(het.plot[[i]]$Estimator, levels = c("PATT-C","PATT"))
  offset <- c("   ") 
  het.plot[[i]]$x <- paste(offset,het.plot[[i]]$x) # make offset in x var name
  
  het.plot[[i]]$order <- 1:nrow(het.plot[[i]])   
  order.dot <- data.frame(x= rep(c("Overall:",
                        #           "  ",
                                   "Sex:",
                       #            "  ",
                                   "Age:",
                       #            "  ",
                                   "Race:",
                        #           "   ",
                                   "Health status:",
                        #           "    ",
                                   "Education:",
                        #           "    ",
                                   "Income:"),2), 
#                           order=c(.5,1.1,1.5,2.1,2.5,4.1,4.5,7.1,7.5,11.1,11.5,15.1,15.5,
#                                   18.5,19.1,19.5,20.1,20.5,22.1,22.5,25.1,25.5,29.1,29.5,33.1,33.5,
#                                   36.5,37.1,37.5,38.1,38.5,40.1,40.5,43.1,43.5,47.1,47.5,51.1,51.5), 
                          order=c(.5,1.5,2.5,4.5,7.5,11.5,15.5,
                                  18.5,19.5,20.5,22.5,25.5,29.5,33.5),
                             #     36.5,37.5,38.5,40.5,43.5,47.5,51.5), 
                          y=NA,
                          Group=NA,
                          Estimator=NA,
                          ci.lower=NA,
                          ci.upper=NA)
  het.plot[[i]] <- rbind(het.plot[[i]],order.dot)
  het.plot[[i]] <-het.plot[[i]][order(het.plot[[i]]$order),]
  het.plot[[i]]$x <- factor(het.plot[[i]]$x,levels=unique(het.plot[[i]]$x)[length(het.plot[[i]]$x):1])
}

# Plot forest plot

ThemeBw1 <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
     legend.position = "right"
    )
}

het.plot.all <- lapply(y.col, function (i) 
  ggplot(het.plot[[i]][], aes(x=x, y=y, ymin = ci.lower, ymax = ci.upper, colour=Estimator)) +
    geom_pointrange(size=1, alpha=0.8) +
    scale_colour_manual(values=c("red","blue"), breaks=levels(het.plot[[i]]$Estimator)) + # change colors for estimators
    coord_flip() +
    geom_line() +
   geom_hline(aes(x=0,yintercept=0), lty=2) +
    ThemeBw1() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Treatment effect") +
    xlab("")) #switch because of the coord_flip() above 

any.visit.plot <- het.plot.all[[1]] + ggtitle("Any ER visit")
num.visit.plot <- het.plot.all[[2]] + ggtitle("# ER visits")
num.out.plot <- het.plot.all[[4]] + ggtitle("# outpatient visits") 
                                              
ggsave(paste0(repo.directory, "plots/any-visit-plot.png"), any.visit.plot)
ggsave(paste0(repo.directory, "plots/num-visit-plot.png"), num.visit.plot)
ggsave(paste0(repo.directory, "plots/num-out-plot.png"), num.out.plot) 