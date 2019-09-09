packages <- c("ggplot2","ggpubr","gridExtra","reshape2","dplyr","MASS","gbm","rpart","foreach","doParallel","downloader","SAScii","RCurl",
              "foreign","plyr","downloader","digest","SuperLearner","class", "randomForest","glmnet","gam","e1071","gbm","xgboost","ROCR","reporttools")

weights <- c("cluster","HMisc","weights") # install cluster -> HMisc -> weights

install.packages(c(packages,weights))