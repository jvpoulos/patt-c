# Make tile plots and side-by-side boxplots for simulation comparisons
######################################################################

load("simulation_res.RData")
library(ggplot2)
library(gridExtra)
color_extremes <- c("yellow", "red")

### compare compliance rate and treatment rate
p1 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits = c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for Treatment", title = "PATT-C")+ guides(fill = guide_colorbar(title = "RMSE"))
p1
p2 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt_unadj)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits = c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for Treatment", title = "PATT")+ guides(fill = guide_colorbar(title = "RMSE"))
p2
ggsave("rmse_ratec_ratet.png", grid.arrange(p1,p2),
       width=11, height=8.5)

### compare compliance rate and RCT eligibility rate
p1 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits =  c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "PATT-C") + guides(fill = guide_colorbar(title = "RMSE"))
p1
p2 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt_unadj)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits =  c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "PATT") + guides(fill = guide_colorbar(title = "RMSE"))
p2
ggsave("rmse_ratec_rates.png", grid.arrange(p1,p2),
       width=11, height=8.5)

library(reshape2); library(dplyr)
res2 <- data.frame(rep(res$rateC, 3), c(res$mse_tpatt, res$mse_tpatt_unadj, res$mse_rct_satt), rep(c("PATT-C", "PATT", "SATT-C"), each = nrow(res)));
colnames(res2) <- c("rateC", "mse", "Estimator")
levels(res2$Estimator) <- list("PATT-C" = 1, "PATT" = 2, "SATT-C" = 3)

### Look at just compliance rate
p1 <- ggplot(res2, aes(x = as.factor(round(rateC,1)), y = sqrt(mse))) + geom_boxplot(aes(color = Estimator)) +labs(x = "Compliance rate", y = "RMSE", title = "Varying compliance rate") + scale_color_brewer(palette="Set1")
ggsave("rmse_boxplots_rateC.png", p1,
       width=11, height=8.5)

## rate of confounding with sample selection 
p1 <- ggplot(res2, aes(x = as.factor(round(RateConS,1)), y = sqrt(mse))) + geom_boxplot(aes(color = Estimator)) +labs(x = "Confounding in sample selection", y = "RMSE", title = "Varying degree of confounding in sample selection") + scale_color_brewer(palette="Set1")
ggsave("rmse_boxplots_ratecons.png", p1,
       width=11, height=8.5)

## rate of confounding with the treatment assignment 
p1 <- ggplot(res2, aes(x = as.factor(round(RateConT,1)), y = sqrt(mse))) + geom_boxplot(aes(color = Estimator)) +labs(x = "Confounding in treatment assignment", y = "RMSE", title = "Varying degree of confounding in treatment assignment") + scale_color_brewer(palette="Set1")
ggsave("rmse_boxplots_ratecont.png", p1,
       width=11, height=8.5)

## rate of confounding with compliance 
p1 <- ggplot(res2, aes(x = as.factor(round(RateConC,1)), y = sqrt(mse))) + geom_boxplot(aes(color = Estimator)) +labs(x = "Confounding in compliance", y = "RMSE", title = "Varying degree of confounding in compliance") + scale_color_brewer(palette="Set1")
ggsave("rmse_boxplots_rateconc.png", p1,
       width=11, height=8.5)