identify_kt <- function(model, ...) {
  tskt <- diff(ts(c(model$kt)))

  par(mfrow=c(1, 2))
  acf(tskt, main="", ...)
  pacf(tskt, main="", ...)
  par(mfrow=c(1, 1))
  TSA::eacf(tskt, 3, 3)
}


forecast_rates <- function(models, until=2050, n_sim=1000, 
                           order_f=NULL, order_m=NULL) {
  h <- until - max(models$Period)
  
  res <- list()
  
  m <- models[["Female"]]
  
  dat <- t(m$data$Dxt/m$data$Ext)
  est <- t(fitted(m))
  fore <- forecast(m, h=h, kt.method="iarima", kt.order=order_f)
  ktf <- c(fore$kt.f$mean)
  fore <- t(fore$rates)
  colnames(fore) <- colnames(dat) <- colnames(est) <- models$Age
  
  sim <- simulate(m, nsim=n_sim, h=h, kt.method="iarima", kt.order=order_f)
  kts <- sim$kt.s$sim[1,,]
  sim <- lapply(1:n_sim, function(x) t(sim$rates[,,x]))
  
  res[["Female"]] <- list(
    Data=dat, Forecast=fore, Fitted=est, Boot=sim, kt.fore=ktf, kt.sim=kts
  )
  
  m <- models[["Male"]]

  dat <- t(m$data$Dxt/m$data$Ext)
  est <- t(fitted(m))
  fore <- forecast(m, h=h, kt.method="iarima", kt.order=order_m)
  ktf <- c(fore$kt.f$mean)
  fore <- t(fore$rates)
  colnames(fore) <- colnames(dat) <- colnames(est) <- models$Age
  
  sim <- simulate(m, nsim=n_sim, h=h, kt.method="iarima", kt.order=order_m)
  kts <- sim$kt.s$sim[1,,]
  sim <- lapply(1:n_sim, function(x) t(sim$rates[,,x]))
  
  res[["Male"]] <- list(
    Data=dat, Forecast=fore, Fitted=est, Boot=sim, kt.fore=ktf, kt.sim=kts
  )
  
  res
}


simulate_incidence <- function(fore, pop.f, pop.m) {
  
  mc <- list()
  
  for (i in 1:length(pop.f$Boot)) {
    p.f <- pop.f$Boot[[i]]
    p.m <- pop.m$Boot[[i]]
    r.f <- fore$Female$Boot[[i]]
    r.m <- fore$Male$Boot[[i]]
    n.f <- rpois(nrow(r.f)*ncol(r.f), p.f*r.f)
    n.f <- matrix(n.f, nrow(r.f), ncol(r.f))
    n.m <- rpois(nrow(r.m)*ncol(r.m), p.m*r.m)
    n.m <- matrix(n.m, nrow(r.m), ncol(r.m))
    mc[[i]] <- list(p.f=p.f, p.m=p.m, n.f=n.f, n.m=n.m)
  }
  
  
  
  list(
    Data=list(p.f=pop.f$Data, p.m=pop.m$Data, 
              n.f=fore$Female$Data*pop.f$Data, n.m=fore$Male$Data*pop.m$Data),
    Fitted=list(p.f=pop.f$Data, p.m=pop.m$Data, 
                n.f=fore$Female$Fitted*pop.f$Data, n.m=fore$Male$Fitted*pop.m$Data),
    Forecast=list(p.f=pop.f$Forecast, p.m=pop.m$Forecast, 
                  n.f=fore$Female$Forecast*pop.f$Forecast, n.m=fore$Male$Forecast*pop.m$Forecast),
    Boot=mc
  )
}

