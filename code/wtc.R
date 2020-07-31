WtC <- function (x, y, c=NULL, weight = NULL, weighty = NULL, weightc = NULL, cluster = NULL, clustery = NULL, clusterc = NULL, samedata = TRUE, 
          alternative = "two.tailed", mean1 = TRUE, bootse = TRUE, 
          bootp = FALSE, bootn = 1000, drops = "pairwise", equivalence=FALSE) {
  ## from wtd.t.test package weights version 1.0
  library(weights)
  if (is.null(weight)) {
    weight <- rep(1, length(x))
  }
  if (is.null(cluster)) {
    cluster <- rep(1, length(x))
  }
  if (is.null(clustery)) {
    clustery <- rep(1, length(x))
  }
  if (bootse == FALSE & bootp == TRUE) 
    warning("bootp can only be used with bootstrapped standard errors")
  if (length(y) != length(x) & length(y) > 1) {
    if (samedata == TRUE) 
      warning("Treating data for x and y separately because they are of different lengths")
    samedata <- FALSE
  }
  if (length(y) == 1) 
    samedata <- FALSE
  if (samedata == TRUE & drops == "pairwise") {
    use <- !is.na(x) & !is.na(y) & !is.na(weight)
    x <- x[use]
    if (length(y) > 1) 
      y <- y[use]
    weight <- weight[use]
  }
  if (is.null(weighty) & samedata == TRUE) {
    weighty <- weight
  }
  if (is.null(weighty) & samedata == FALSE & length(y) > 1) {
    warning("y has no weights, weights for y are assumed to be 1")
    weighty <- rep(1, length(y))
  }
  if (mean1 == TRUE) {
    weight <- weight/mean(weight, na.rm = TRUE)
    if (length(y) > 1) 
      weighty <- weighty/mean(weighty, na.rm = TRUE)
  }
  
  n <- length(unique(cluster))
  m.bar <- sum(tapply(weight, cluster, FUN=sum, na.rm = TRUE))/n
  x.bar <- tapply(x, cluster, FUN=mean, na.rm = TRUE)
  mx <- sum(x.bar*tapply(weight, cluster, FUN=sum, na.rm = TRUE))/sum(tapply(weight, cluster, FUN=sum, na.rm = TRUE))
  vx <- (sum(tapply(weight, cluster, FUN=sum, na.rm = TRUE)*x.bar**2)/m.bar - n*wtd.mean(x, weight, na.rm = TRUE)**2)/(n-1)
 
  if (length(y) > 1) {

    n2 <- length(unique(clustery))
    m.bar2 <- sum(tapply(weighty, clustery, FUN=sum, na.rm = TRUE))/n2
    y.bar <- tapply(y, clustery, FUN=mean, na.rm = TRUE)
    my <- sum(y.bar*tapply(weighty, clustery, FUN=sum, na.rm = TRUE))/sum(tapply(weighty, clustery, FUN=sum, na.rm = TRUE))
    vy <- (sum(tapply(weighty, clustery, FUN=sum, na.rm = TRUE)*y.bar**2)/m.bar2 - n2*wtd.mean(y, weighty, na.rm = TRUE)**2)/(n2-1)
      
    dif <- mx - my
    sxy <- sqrt((vx/n) + (vy/n2))
    
    if(!is.null(c)){
      n3 <- length(unique(clusterc))
      m.bar3 <- sum(tapply(weightc, clusterc, FUN=sum, na.rm = TRUE))/n3
      c.bar <- tapply(c, clusterc, FUN=mean, na.rm = TRUE)
      mc <- sum(c.bar*tapply(weightc, clusterc, FUN=sum, na.rm = TRUE))/sum(tapply(weightc, clusterc, FUN=sum, na.rm = TRUE))
      vc <- (sum(tapply(weightc, clusterc, FUN=sum, na.rm = TRUE)*c.bar**2)/m.bar3 - n3*wtd.mean(c, weightc, na.rm = TRUE)**2)/(n3-1)
      
      dif <- (mx - my)/mc
      sxy <- sqrt((vx/n) + (vy/n2) + (vc/n3))
    }
    if (bootse == TRUE) {

      samps1 <- lapply(1:bootn, function(g) sample.int(length(unique(cluster)), replace = TRUE, 
                                                   prob = tapply(weight, cluster, FUN=sum, na.rm = TRUE)))
      
      samps2 <- lapply(1:bootn, function(g) sample.int(length(unique(clustery)), replace = TRUE, 
                                                   prob = tapply(weighty, clustery, FUN=sum, na.rm = TRUE)))
      
      sepests1 <- sapply(samps1, function(q) mean(x.bar[q], na.rm=TRUE))
      sepests2 <- sapply(samps2, function(q) mean(y.bar[q], na.rm=TRUE))
      
      sxy <- sqrt(var(sepests1 - sepests2, na.rm = TRUE))
      
      if(!is.null(c)){
        samps3 <- lapply(1:bootn, function(g) sample.int(length(unique(clusterc)), replace = TRUE, 
                                                     prob = tapply(weightc, clusterc, FUN=sum, na.rm = TRUE)))
        
        sepests3 <- sapply(samps3, function(q) mean(c.bar[q], na.rm=TRUE))
        
        sxy <- sqrt(var((sepests1 - sepests2)/sepests3, na.rm = TRUE))
        
      }
    }
    df <- (((vx/n) + (vy/n2))^2)/((((vx/n)^2)/(n - 1)) + 
                                    ((vy/n2)^2/(n2 - 1)))
    
    t <- (mx - my)/sxy
    p.value <- (1 - pt(abs(t), df)) * 2
    if(!is.null(c)){
      df <- (((vx/n) + (vy/n2) + (vc/n3))^2)/((((vx/n)^2)/(n - 1)) + 
                                      ((vy/n2)^2/(n2 - 1)) +
                                        ((vc/n3)^2/(n3 - 1)))
      t <- ((mx - my)/mc)/sxy
      p.value <- (1 - pt(abs(t), df)) * 2
    }
    cint <- NULL
    if (equivalence){
      epsilon <-1
      conf.level <- 0.95

      p.value <- as.numeric(pt((epsilon - abs(mx-my))/sxy, df, lower.tail = FALSE))
    }
    if (alternative == "greater") 
      p.value <- pt(t, df, lower.tail = FALSE)
    if (alternative == "less") 
      p.value <- pt(t, df, lower.tail = TRUE)
    if (bootp == TRUE & bootse == TRUE) 
      p.value <- 2 * min(c(sum(sepests1 > sepests2 & !is.na(sepests1))/sum(!is.na(sepests1)), 
                           sum(sepests1 < sepests2 & !is.na(sepests1))/sum(!is.na(sepests1))))
    if (bootp == TRUE & bootse == TRUE & alternative == "greater") 
      p.value <- sum(sepests1 > sepests2 & !is.na(sepests1))/sum(!is.na(sepests1))
    if (bootp == TRUE & bootse == TRUE & alternative == "less") 
      p.value <- sum(sepests1 < sepests2 & !is.na(sepests1))/sum(!is.na(sepests1))
    coef <- c(t, df, p.value)
    out2 <- c(dif, mx, my, sxy)
    names(coef) <- c("t.value", "df", "p.value")
    names(out2) <- c("Difference", "Mean.x", "Mean.y", "Std. Err")
    out <- list("Two Sample Weighted T-Test (Welch)", coef, 
                out2)
    names(out) <- c("test", "coefficients", "additional")
  }
  out
}