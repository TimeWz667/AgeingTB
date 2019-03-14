extract_parameters <- function(models, n_boot, ps=c(0.025, 0.05, 0.95, 0.975)) {
  res <- list()
  
  for (sex in c("Female", "Male")) {
    m <- models[[sex]]
    boot.f <- bootstrap(m, nBoot=n_boot, type="semiparametric")
  
    boot.ax <- sapply(boot.f$bootParameters, function(x) x$ax)
    boot.ax <- t(apply(boot.ax, 1, quantile, p=ps))
    colnames(boot.ax) <- paste0("Q", ps*100)
    boot.ax <- data.frame(Mean=m$ax, boot.ax)
    rownames(boot.ax) <- models$Age
    
    boot.bx <- sapply(boot.f$bootParameters, function(x) x$bx)
    boot.bx <- t(apply(boot.bx, 1, quantile, p=ps))
    colnames(boot.bx) <- paste0("Q", ps*100)
    boot.bx <- data.frame(Mean=c(m$bx), boot.bx)
    rownames(boot.bx) <- models$Age
    
    boot.kt <- sapply(boot.f$bootParameters, function(x) x$kt)
    boot.kt <- t(apply(boot.kt, 1, quantile, p=ps))
    colnames(boot.kt) <- paste0("Q", ps*100)
    boot.kt <- data.frame(Mean=c(m$kt), boot.kt)
    rownames(boot.kt) <- models$Period
    
    boot.fitted <- simulate(boot.f)$fitted
    ftt <- array(0, c(dim(boot.fitted)[1:2], 3))
    ftt[,, 1] <- apply(boot.fitted, c(1, 2), mean)
    ftt[,, 2] <- apply(boot.fitted, c(1, 2), quantile, p=0.025)
    ftt[,, 3] <- apply(boot.fitted, c(1, 2), quantile, p=0.975)
    ftt <- aperm(ftt, c(2, 1, 3))
    
    dimnames(ftt)[[1]] <- models$Period
    dimnames(ftt)[[2]] <- models$Age
    dimnames(ftt)[[3]] <- c("Mean", "Lower", "Upper")
    
    res[[sex]] <- list(
      ax=boot.ax,
      bx=boot.bx,
      kt=boot.kt,
      fitted=ftt
    )
  }

  res
}


extract_forecast_kt <- function(forecasts, ps=c(0.025, 0.05, 0.95, 0.975)) {
  res <- list()
  
  for (sex in c("Female", "Male")) {
    f <- forecasts[[sex]]

    boot.kt <- t(apply(f$kt.sim, 1, quantile, p=ps))
    colnames(boot.kt) <- paste0("Q", ps*100)
    res[[sex]] <- data.frame(Mean=c(f$kt.fore), boot.kt)
  }
  
  res
}


