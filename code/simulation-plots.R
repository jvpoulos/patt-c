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
p1 <- ggplot(res2, aes(x = as.factor(round(rateC,1)), y = sqrt(mse))) + geom_boxplot(aes(color = Estimator)) +labs(x = "Compliance rate", y = "RMSE", title = "RMSE of Sample and Population Treatment Effect Estimators") + scale_color_brewer(palette="Set1")
ggsave("rmse_boxplots.png",
       width=11, height=8.5)