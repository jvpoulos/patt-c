# Make tile plots and side-by-side boxplots for simulation comparisons
######################################################################

load(paste0(repo.directory , "results/simulation_res.RData"))
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(dplyr)

color_extremes <- c("yellow", "red")

### compare compliance rate and treatment rate across estimators

res.long <-  melt(res[c("rateC","rateS","rateT",      
                         "mse_tpatt","mse_tpatt_unadj","mse_rct_sate")], id.vars = c("rateC","rateS","rateT"))

rmse.limits <- range(sqrt(res.long$value), na.rm=TRUE)

p1 <- ggplot(res.long[res.long$variable=="mse_tpatt",], aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) + 
  geom_tile(aes(fill = sqrt(value)), colour = "yellow") + scale_fill_gradientn(colours = color_extremes, limits=round(rmse.limits), na.value = "white") + labs(x = "Compliance rate", y = "Treatment rate", title = "PATT-C")+ 
  guides(fill = guide_colorbar(title = "RMSE")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background=element_rect(fill="white", colour="white"))

p2 <- ggplot(res.long[res.long$variable=="mse_tpatt_unadj",], aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) +
  geom_tile(aes(fill = sqrt(value)), colour = "yellow") + scale_fill_gradientn(colours = color_extremes, limits=round(rmse.limits), na.value = "white") + labs(x = "Compliance rate", y = "Treatment rate", title = "PATT")+ 
  guides(fill = guide_colorbar(title = "RMSE")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background=element_rect(fill="white", colour="white"))

p3 <- ggplot(res.long[res.long$variable=="mse_rct_sate",], aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) +
  geom_tile(aes(fill = sqrt(value)), colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits=round(rmse.limits), na.value = "white") + labs(x = "Compliance rate", y = "Treatment rate", title = "CACE")+ 
  guides(fill = guide_colorbar(title = "RMSE")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background=element_rect(fill="white", colour="white")) + theme(legend.position="right")

legend <- get_legend(p3) # each plot has same limits

ggsave(paste0(repo.directory , "plots/rmse_ratec_ratet.png"), 
       ggarrange(p1, p2, p3, as_ggplot(legend), ncol=2, nrow=2, common.legend = TRUE, legend="none"),
       width=11, height=8.5)

### compare compliance rate and RCT eligibility rate

p1 <- ggplot(res.long[res.long$variable=="mse_tpatt",], aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) + 
  geom_tile(aes(fill = sqrt(value)), colour = "yellow") + scale_fill_gradientn(colours = color_extremes, limits=round(rmse.limits), na.value = "white") + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "PATT-C")+ 
  guides(fill = guide_colorbar(title = "RMSE")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background=element_rect(fill="white", colour="white"))

p2 <- ggplot(res.long[res.long$variable=="mse_tpatt_unadj",], aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) +
  geom_tile(aes(fill = sqrt(value)), colour = "yellow") + scale_fill_gradientn(colours = color_extremes, limits=round(rmse.limits), na.value = "white") + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "PATT")+ 
  guides(fill = guide_colorbar(title = "RMSE")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background=element_rect(fill="white", colour="white"))

p3 <- ggplot(res.long[res.long$variable=="mse_rct_sate",], aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) +
  geom_tile(aes(fill = sqrt(value)), colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits=round(rmse.limits), na.value = "white") + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "CACE")+ 
  guides(fill = guide_colorbar(title = "RMSE")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background=element_rect(fill="white", colour="white")) + theme(legend.position="right")

legend <- get_legend(p3)

ggsave(paste0(repo.directory , "plots/rmse_ratec_rates.png"), 
       ggarrange(p1, p2, p3, as_ggplot(legend), ncol=2, nrow=2, common.legend = TRUE, legend="none"),
       width=11, height=8.5)

res2 <- data.frame(rep(res$rateC, 3), c(res$mse_tpatt, res$mse_tpatt_unadj, res$mse_rct_sate), rep(c("PATT-C", "PATT", "CACE"), each = nrow(res)));
colnames(res2) <- c("rateC", "mse", "Estimator")
levels(res2$Estimator) <- list("PATT-C" = 1, "PATT" = 2, "CACE" = 3)

res3 <- data.frame(rep(res$RateConS, 3), c(res$mse_tpatt, res$mse_tpatt_unadj, res$mse_rct_sate), rep(c("PATT-C", "PATT", "CACE"), each = nrow(res)));
colnames(res3) <- c("RateConS", "mse", "Estimator")
levels(res3$Estimator) <- list("PATT-C" = 1, "PATT" = 2, "CACE" = 3)

res4 <- data.frame(rep(res$RateConT, 3), c(res$mse_tpatt, res$mse_tpatt_unadj, res$mse_rct_sate), rep(c("PATT-C", "PATT", "CACE"), each = nrow(res)));
colnames(res4) <- c("RateConT", "mse", "Estimator")
levels(res4$Estimator) <- list("PATT-C" = 1, "PATT" = 2, "CACE" = 3)

res5 <- data.frame(rep(res$RateConC, 3), c(res$mse_tpatt, res$mse_tpatt_unadj, res$mse_rct_sate), rep(c("PATT-C", "PATT", "CACE"), each = nrow(res)));
colnames(res5) <- c("RateConC", "mse", "Estimator")
levels(res5$Estimator) <- list("PATT-C" = 1, "PATT" = 2, "CACE" = 3)

### Look at just compliance rate
p1 <- ggplot(res2, aes(x = as.factor(round(rateC,1)), y = sqrt(mse))) + 
  geom_boxplot(aes(color = Estimator)) +labs(x = "Compliance rate", y = "RMSE", title = "RMSE of estimators when varying compliance rate") + 
  scale_color_brewer(palette="Set1") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(c(0,4))
ggsave(paste0(repo.directory , "plots/rmse_boxplots_rateC.png"), p1,
       width=11, height=8.5)

## rate of confounding with sample selection 
p1 <- ggplot(res3, aes(x = as.factor(round(RateConS,1)), y = sqrt(mse))) + 
  geom_boxplot(aes(color = Estimator)) +
  labs(x = "Confounding in sample selection", y = "RMSE", title = "RMSE of estimators when varying degree of confounding in sample selection") + 
  scale_color_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5)) + ylim(c(0,4))
ggsave(paste0(repo.directory , "plots/rmse_boxplots_RateConS.png"), p1,
       width=11, height=8.5)

## rate of confounding with the treatment assignment 
p1 <- ggplot(res4, aes(x = as.factor(round(RateConT,1)), y = sqrt(mse))) + 
  geom_boxplot(aes(color = Estimator)) +
  labs(x = "Confounding in treatment assignment", y = "RMSE", title = "RMSE of estimators when varying degree of confounding in treatment assignment") + 
  scale_color_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5)) + ylim(c(0,4))
ggsave(paste0(repo.directory , "plots/rmse_boxplots_RateConT.png"), p1,
       width=11, height=8.5)

## rate of confounding with compliance 
p1 <- ggplot(res5, aes(x = as.factor(round(RateConC,1)), y = sqrt(mse))) + 
  geom_boxplot(aes(color = Estimator)) +labs(x = "Confounding in compliance", y = "RMSE", title = "RMSE of estimators when varying degree of confounding in compliance") + 
  scale_color_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5)) + ylim(c(0,4))
ggsave(paste0(repo.directory , "plots/rmse_boxplots_RateConC.png"), p1,
       width=11, height=8.5)