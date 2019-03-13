extract_parameters <- function(models, n_boot, ps=c(0.025, 0.05, 0.95, 0.975)) {
  res <- list()
  
  for (sex in c("Female", "Male")) {
    m <- models[[sex]]
    boot.f <- bootstrap(m, nBoot=n_boot, type="semiparametric")
  
    boot.ax <- sapply(boot.f$bootParameters, function(x) x$ax)
    boot.ax <- t(apply(boot.ax, 1, quantile, p=ps))
    colnames(boot.ax) <- paste0("Q", ps*100)
    boot.bx <- sapply(boot.f$bootParameters, function(x) x$bx)
    boot.bx <- t(apply(boot.bx, 1, quantile, p=ps))
    colnames(boot.bx) <- paste0("Q", ps*100)
    boot.kt <- sapply(boot.f$bootParameters, function(x) x$kt)
    boot.kt <- t(apply(boot.kt, 1, quantile, p=ps))
    colnames(boot.kt) <- paste0("Q", ps*100)
    
    res[[sex]] <- list(
      ax=data.frame(mean=m$ax, boot.ax),
      bx=data.frame(mean=c(m$bx), boot.bx),
      kt=data.frame(mean=c(m$kt), boot.kt)
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
    res[[sex]] <- data.frame(mean=c(f$kt.fore), boot.kt)
  }
  
  res
}
