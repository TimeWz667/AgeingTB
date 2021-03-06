summary_pop_with_age <- function(p.f, p.m, agp) {
  p.total <- p.f + p.m
  p.rsum <- rowSums(p.total)
  
  chd <- 1:3; young <-4:7; mid <- 8:13; old <- 14:15 
  
  res <- data.frame(Year=as.numeric(rownames(p.total)), 
                    TotalN=p.rsum)
  
  for (a in names(agp)) {
    ind <- agp[[a]]
    
    n <- rowSums(p.total[, ind])
    res[paste0(a, "N")] <- n
  }
  
  rownames(res) <- NULL
  res
}


summary_with_age <- function(inc, agp) {
  n.total <- inc$n.f + inc$n.m
  p.total <- inc$p.f + inc$p.m
  
  n.sums <- rowSums(n.total)
  p.sums <- rowSums(p.total)
  
  res <- data.frame(Year=as.numeric(rownames(n.total)), 
                    TotalN=n.sums, TotalR=n.sums/p.sums*1e5)
  
  for (a in names(agp)) {
    ind <- agp[[a]]
    
    n <- rowSums(n.total[, ind])
    pop <- rowSums(p.total[, ind])
    res[paste0(a, "N")] <- n
    res[paste0(a, "R")] <- n/pop * 1e5
    res[paste0(a, "Pr")] <- n/n.sums
    res[paste0(a, "Fr")] <- n/p.sums * 1e5
  }
  
  rownames(res) <- NULL
  res
}


summarise_all_age <- function(sims, agp, q=0.95) {
  fore <- summary_with_age(sims$Forecast, agp)
  
  
  mc <- array(0, c(dim(fore), length(sims$Boot)))
  
  for (i in 1:length(sims$Boot)) {
    summ <- summary_with_age(sims$Boot[[i]], agp)
    mc[,, i] <- as.matrix(summ)
  }
  
  dimnames(mc)[[1]] <- rownames(fore)
  dimnames(mc)[[2]] <- colnames(fore)
  
  mc.summary <- array(0, c(dim(fore), 3))
  mc.summary[,, 1] <- apply(mc, c(1, 2), mean)
  mc.summary[,, 2] <- apply(mc, c(1, 2), function(x) quantile(x, .5-q/2))
  mc.summary[,, 3] <- apply(mc, c(1, 2), function(x) quantile(x, .5+q/2))

  dimnames(mc.summary)[[1]] <- rownames(fore)
  dimnames(mc.summary)[[2]] <- colnames(fore)
  dimnames(mc.summary)[[3]] <- c("Mean", "Lower", "Upper")
  
  list(
    Data=summary_with_age(sims$Data, agp),
    Fitted=summary_with_age(sims$Fitted, agp),
    Forecast=fore,
    Boot=mc.summary
  )
}


summary_with_sex <- function(inc) {
  n.total <- inc$n.f + inc$n.m
  p.total <- inc$p.f + inc$p.m
  
  n.sums <- rowSums(n.total)
  p.sums <- rowSums(p.total)
  
  res <- data.frame(Year=as.numeric(rownames(n.total)), 
                    TotalN=n.sums, TotalR=n.sums/p.sums*1e5)
  
  n <- rowSums(inc$n.f)
  pop <- rowSums(inc$p.f)
  res["FemaleN"] <- n
  res["FemaleR"] <- n/pop * 1e5
  res["FemalePr"] <- n/n.sums
  res["FemaleFr"] <- n/p.sums * 1e5

  n <- rowSums(inc$n.m)
  pop <- rowSums(inc$p.m)
  res["MaleN"] <- n
  res["MaleR"] <- n/pop * 1e5
  res["MalePr"] <- n/n.sums
  res["MaleFr"] <- n/p.sums * 1e5
  
  rownames(res) <- NULL
  res
}


summarise_reduction_age <- function(sims, baseline=2015, q=0.95) {
  baseline <- as.character(baseline)
  r0s <- with(sims$Data, {(n.f[baseline,]+n.m[baseline,])/(p.f[baseline,]+p.m[baseline,])})
  r0 <- with(sims$Data, {sum(n.f[baseline,]+n.m[baseline,])/sum(p.f[baseline,]+p.m[baseline,])})
  red <- 1-with(sims$Forecast, {rowSums(n.f+n.m)/rowSums(p.f+p.m)})/r0
  
  
  fore <- -(t(with(sims$Forecast, {(n.f+n.m)/(p.f+p.m)})) - r0s)/r0s
  fore <- t(fore)
  
  mc <- array(0, c(dim(fore), length(sims$Boot)))
  
  for (i in 1:length(sims$Boot)) {
    summ <- -(t(with(sims$Boot[[i]], {(n.f+n.m)/(p.f+p.m)})) - r0s)/r0s
    mc[,, i] <- t(summ)
  }
  
  dimnames(mc)[[1]] <- rownames(fore)
  dimnames(mc)[[2]] <- colnames(fore) 
  
  
  mc.summary <- array(0, c(dim(fore), 3))
  mc.summary[,, 1] <- apply(mc, c(1, 2), mean)
  mc.summary[,, 2] <- apply(mc, c(1, 2), function(x) quantile(x, .5-q/2))
  mc.summary[,, 3] <- apply(mc, c(1, 2), function(x) quantile(x, .5+q/2))
  
  dimnames(mc.summary)[[1]] <- rownames(fore)
  dimnames(mc.summary)[[2]] <- colnames(fore)
  dimnames(mc.summary)[[3]] <- c("Mean", "Lower", "Upper")
  
  list(Base=r0s, All=red, Forecast=fore, Boot=mc.summary)
}



summarise_reduction_all <- function(sims, baseline=2015, q=0.95) {
  baseline <- as.character(baseline)
  r0 <- with(sims$Data, {sum(n.f[baseline,]+n.m[baseline,])/sum(p.f[baseline,]+p.m[baseline,])})
  
  
  fore <- 1-with(sims$Forecast, {rowSums(n.f+n.m)/rowSums(p.f+p.m)})/r0
  
  mc <- sapply(sims$Boot, function(x) 1-with(x, {rowSums(n.f+n.m)/rowSums(p.f+p.m)})/r0)
    
  
  data.frame(
    Year=as.numeric(names(fore)),
    Base=r0*1e5,
    Mean=fore,
    Lower=apply(mc, 1, quantile, p=.5-q/2),
    Upper=apply(mc, 1, quantile, p=.5+q/2)
  )

}


summarise_paf <- function(sims1, sims0, q=0.95) {
  fore <- 1-with(sims0$Forecast, {rowSums(n.f+n.m)})/with(sims1$Forecast, {rowSums(n.f+n.m)})
  
  mc1 <- sapply(sims1$Boot, function(x) with(x, {rowSums(n.f+n.m)}))
  mc0 <- sapply(sims0$Boot, function(x) with(x, {rowSums(n.f+n.m)}))
  mc <- 1 - mc0/mc1
  
  data.frame(
    Year=as.numeric(names(fore)),
    Mean=fore,
    Lower=apply(mc, 1, quantile, p=.5-q/2),
    Upper=apply(mc, 1, quantile, p=.5+q/2)
  )
  
}
