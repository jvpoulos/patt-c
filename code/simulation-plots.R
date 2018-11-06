# Make tile plots and side-by-side boxplots for simulation comparisons
######################################################################

load("simulation_res.RData")
library(ggplot2)
library(gridExtra)
color_extremes <- c("yellow", "red")

### compare compliance rate and treatment rate
p1 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits = c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for Treatment", title = "PATT Adjusted")+ guides(fill = guide_colorbar(title = "RMSE"))
p1
p2 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt_unadj)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits = c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for Treatment", title = "PATT Unadjusted")+ guides(fill = guide_colorbar(title = "RMSE"))
p2
pdf("rmse_ratec_ratet.pdf", width = 12)
grid.arrange(p1, p2)
dev.off()


### compare compliance rate and RCT eligibility rate
p1 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits =  c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "PATT Adjusted") + guides(fill = guide_colorbar(title = "RMSE"))
p1
p2 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) + geom_tile(aes(fill = sqrt(mse_tpatt_unadj)),     colour = "yellow")+ scale_fill_gradientn(colours = color_extremes, limits =  c(0, 2)) + labs(x = "Compliance rate", y = "% Eligible for RCT", title = "PATT Unadjusted") + guides(fill = guide_colorbar(title = "RMSE"))
p2
pdf("rmse_ratec_rates.pdf", width = 12)
grid.arrange(p1, p2)
dev.off()

library(reshape2); library(dplyr)
res2 <- data.frame(rep(res$rateC, 3), c(res$mse_tpatt, res$mse_tpatt_unadj, res$mse_rct_satt), rep(c("Adjusted", "Unadjusted", "SATT"), each = nrow(res)));
colnames(res2) <- c("rateC", "mse", "Estimator")
levels(res2$Estimator) <- list("Adjusted" = 1, "Unadjusted" = 2, "SATT" = 3)
### Look at just compliance rate
p1 <- ggplot(res2, aes(x = as.factor(round(rateC,1)), y = sqrt(mse))) + geom_boxplot(aes(color = Estimator)) +labs(x = "Compliance rate", y = "RMSE", title = "RMSE of PATT Estimators") + scale_color_brewer(palette="Set1")
p1
pdf("rmse_boxplots.pdf", width = 12)
p1
dev.off()

