WtC <- function (x, y = 0, weight = NULL, weighty = NULL, cluster = NULL, clustery = NULL, samedata = TRUE, 
          alternative = "two.tailed", mean1 = TRUE, bootse = TRUE, 
          bootp = TRUE, bootn = 1000, drops = "pairwise") {
  ## from wtd.t.test package weights version 1.0
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
  
  #n <- sum(weight[!is.na(x)], na.rm = TRUE)
  n <- length(unique(cluster))
  #mx <- wtd.mean(x, weight, na.rm = TRUE)
  m.bar <- sum(tapply(weight, cluster, FUN=sum, na.rm = TRUE))/n
  mx <- sum(tapply(x, cluster, FUN=mean, na.rm = TRUE)*tapply(weight, cluster, FUN=sum, na.rm = TRUE))/sum(tapply(weight, cluster, FUN=sum, na.rm = TRUE))
  #vx <- wtd.var(x, weight, na.rm = TRUE)
  vx <- (sum(tapply(weight, cluster, FUN=sum, na.rm = TRUE)*tapply(x, cluster, FUN=mean, na.rm = TRUE)**2)/m.bar - n*wtd.mean(x, weight, na.rm = TRUE)**2)/(n-1)
  if (length(y) == 1) {
    dif <- mx - y
    sx <- sqrt(vx)
    se <- sx/sqrt(n)
    if (bootse == TRUE) {
      samps <- lapply(1:bootn, function(g) sample(1:length(x), 
                                                  round(sum(weight, na.rm = TRUE), 0), replace = TRUE, 
                                                  prob = weight))
      sepests <- sapply(samps, function(q) mean(x[q], na.rm = TRUE)) - 
        y
      se <- sqrt(var(sepests))
    }
    t <- (mx - y)/se
    df <- n - 1
    p.value <- (1 - pt(abs(t), df)) * 2
    if (alternative == "greater") 
      p.value <- pt(t, df, lower.tail = FALSE)
    if (alternative == "less") 
      p.value <- pt(t, df, lower.tail = TRUE)
    if (bootp == TRUE & bootse == TRUE) 
      p.value <- 2 * min(c(sum(sepests > y & !is.na(sepests))/sum(!is.na(sepests)), 
                           sum(sepests < y & !is.na(sepests))/sum(!is.na(sepests))))
    if (bootp == TRUE & bootse == TRUE & alternative == "greater") 
      p.value <- sum(sepests > y & !is.na(sepests))/sum(!is.na(sepests))
    if (bootp == TRUE & bootse == TRUE & alternative == "less") 
      p.value <- sum(sepests < y & !is.na(sepests))/sum(!is.na(sepests))
    coef <- c(t, df, p.value)
    out2 <- c(dif, mx, y, se)
    names(coef) <- c("t.value", "df", "p.value")
    names(out2) <- c("Difference", "Mean", "Alternative", 
                     "Std. Err")
    out <- list("One Sample Weighted T-Test", coef, out2)
    names(out) <- c("test", "coefficients", "additional")
  }
  if (length(y) > 1) {

    # n2 <- sum(weighty[!is.na(y)], na.rm = TRUE)
    n2 <- length(unique(clustery))
    m.bar2 <- sum(tapply(weighty, clustery, FUN=sum, na.rm = TRUE))/n2
    #my <- wtd.mean(y, weighty, na.rm = TRUE)
    #my <- sum()
    my <- sum(tapply(y, clustery, FUN=mean, na.rm = TRUE)*tapply(weighty, clustery, FUN=sum, na.rm = TRUE))/sum(tapply(weighty, clustery, FUN=sum, na.rm = TRUE))
    #vy <- wtd.var(y, weighty, na.rm = TRUE)
    vy <- (sum(tapply(weighty, clustery, FUN=sum, na.rm = TRUE)*tapply(y, clustery, FUN=mean, na.rm = TRUE)**2)/m.bar2 - n2*wtd.mean(y, weighty, na.rm = TRUE)**2)/(n2-1)
      
    dif <- mx - my
    sxy <- sqrt((vx/n) + (vy/n2))
    if (bootse == TRUE) {

      # samps1 <- lapply(1:bootn, function(g) sample(1:length(x),
      #                                              round(sum(weight, na.rm = TRUE), 0), replace = TRUE,
      #                                              prob = weight))
      samps1 <- lapply(1:bootn, function(g) sample(unique(cluster), 
                                                   size=length(unique(cluster)), replace = TRUE, 
                                                   prob = tapply(weight, cluster, FUN=sum, na.rm = TRUE)))
      
      # samps2 <- lapply(1:bootn, function(g) sample(1:length(y), 
      #                                              round(sum(weighty, na.rm = TRUE), 0), replace = TRUE, 
      #                                              prob = weighty))
      
      samps2 <- lapply(1:bootn, function(g) sample(unique(clustery), 
                                                   size=length(unique(clustery)), replace = TRUE, 
                                                   prob = tapply(weighty, clustery, FUN=sum, na.rm = TRUE)))
      
      sepests1 <- sapply(samps1, function(q) mean(x[q], 
                                                  na.rm = TRUE))
      sepests2 <- sapply(samps2, function(q) mean(y[q], 
                                                  na.rm = TRUE))
      sxy <- sqrt(var(sepests1 - sepests2, na.rm = TRUE))
    }
    df <- (((vx/n) + (vy/n2))^2)/((((vx/n)^2)/(n - 1)) + 
                                    ((vy/n2)^2/(n2 - 1)))
    t <- (mx - my)/sxy
    p.value <- (1 - pt(abs(t), df)) * 2
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