fit_lcm <- function(inc, pop, ages, years, tag, country="Taiwan") {

  dat <- demogdata(t(inc/pop), t(pop), ages=ages, years=years, 
                   type="mortality", label=country, name=tag)
  dat <- StMoMoData(dat)
  model <- fit(lc(), dat, verbose=F)
  model
}


estimate <- function(i.f, i.m, p.f, p.m, country="Taiwan") {
  ages <- 1:ncol(i.f)
  years <- as.integer(rownames(i.f))
  
  list(
    Age=colnames(i.f),
    Period=years,
    Female=fit_lcm(i.f, p.f$Data, ages, years, country=country, tag="Female"),
    Male=fit_lcm(i.m, p.m$Data, ages, years, country=country, tag="Male")
  )
}