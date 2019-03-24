library(ggplot2)
library(reshape2)
library(gridExtra)


vis_age_struture <- function(pop.f, pop.m, year.end, agp, agl) {
  
  pop.data <- summary_pop_with_age(pop.f$Data, pop.m$Data, agp)
  pop.fore <- summary_pop_with_age(pop.f$Forecast, pop.m$Forecast, agp)
  pop.fore <- subset(pop.fore, Year <=year.end)
  
  dat <- data.frame(
    Year = rep(pop.data$Year, 4),
    Age = factor(rep(agl, each=nrow(pop.data)), 
                 levels=agl),
    N = unlist(c(pop.data[, paste0(names(agp), "N")]))*1e-6
  )
  
  fore <- data.frame(
    Year = rep(pop.fore$Year, 4),
    Age = factor(rep(agl, each=nrow(pop.fore)), 
                 levels=agl),
    N = unlist(c(pop.fore[, paste0(names(agp), "N")]))*1e-6
  )
  
  
  g <- ggplot(data=dat, aes(x=Year, y=N)) +
    geom_bar(aes(fill=Age), width=1, position=position_stack(reverse=TRUE), stat="identity") +
    geom_bar(data=fore, stat="identity", width=1, position=position_stack(reverse=TRUE), 
             aes(fill=Age, linetype="Forecast")) +
    geom_vline(aes(xintercept=2018.4), linetype=2) + 
    geom_text(aes(x=2019, y=25, label="Forecast"), hjust=0, vjust=0) +
    geom_text(aes(x=2018, y=25, label="Data"), hjust=1, vjust=0) +
    scale_y_continuous("Population (million)", breaks=seq(0, 25, by=5), limits=c(0, 26)) +
    scale_x_continuous("Year", breaks=seq(2005, year.end, by=5)) +
    # labs(tag="A", title="Population by age group") +
    scale_fill_grey("Age", start=0.8, end=0.3) +
    guides(fill=guide_legend(reverse=F), linetype="none") +
    theme_minimal()
  g
}


vis_parameters <- function(pars, kts, year=2035, l_i="Q2.5", u_i="Q97.5", combine=T) {
  ages <- factor(rownames(pars$Female$ax), levels=rownames(pars$Female$ax))
  
  ax <- rbind(
    data.frame(Age=ages, Sex="Female", pars$Female$ax[,c("Mean", l_i, u_i)]),
    data.frame(Age=ages, Sex="Male", pars$Male$ax[,c("Mean", l_i, u_i)])
  )
  names(ax) <- c("Age", "Sex", "Mean", "Lower", "Upper")
  
  g.ax <- ggplot(data=ax, aes(x=Age)) +
    geom_point(aes(y=Mean), size=0.5) +
    geom_errorbar(aes(ymax=Lower, ymin=Upper, width=0.3)) +
    facet_wrap(~Sex, nrow=2) +
    labs(x="Age", y=NULL, title=expression("Age Effect ("*alpha["age"]*")")) + 
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  
  bx <- rbind(
    data.frame(Age=ages, Sex="Female", pars$Female$bx[,c("Mean", l_i, u_i)]),
    data.frame(Age=ages, Sex="Male", pars$Male$bx[,c("Mean", l_i, u_i)])
  )
  names(bx) <- c("Age", "Sex", "Mean", "Lower", "Upper")
  
  g.bx <- ggplot(data=bx, aes(x=Age)) +
    geom_point(aes(y=Mean), size=0.5) +
    geom_errorbar(aes(ymax=Lower, ymin=Upper, width=0.3)) +
    facet_wrap(~Sex, nrow=2) +
    labs(x="Age", y=NULL, title=expression("Age-Period Interaction ("*beta["age"]*")")) + 
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  
  
  years <- as.numeric(rownames(pars$Female$kt))
  
  kt <- rbind(
    data.frame(Year=years, Sex="Female", pars$Female$kt[,c("Mean", l_i, u_i)]),
    data.frame(Year=years, Sex="Male", pars$Male$kt[,c("Mean", l_i, u_i)])
  )
  names(kt) <- c("Year", "Sex", "Mean", "Lower", "Upper")
  
  years <- as.numeric(rownames(kts$Female))
  
  kt.fore <- rbind(
    data.frame(Year=years, Sex="Female", kts$Female[,c("Mean", l_i, u_i)]),
    data.frame(Year=years, Sex="Male", kts$Female[,c("Mean", l_i, u_i)])
  )
  names(kt.fore) <- c("Year", "Sex", "Mean", "Lower", "Upper")
  kt.fore <- subset(kt.fore, Year <= year)
  
  
  fore_text <- data.frame(
    label1 = c("Forecasts", ""),
    label2 = c("Fitted", ""),
    Sex   = c("Female", "Male")
  )
  
  g.kt <- ggplot(data=kt, aes(x=Year)) +
    geom_point(aes(y=Mean), size=0.5) +
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.3), na.rm=TRUE) +
    geom_line(data=kt.fore, aes(y=Mean)) +
    geom_ribbon(data=kt.fore, aes(ymin=Lower, ymax=Upper), alpha=0.2) +
    facet_wrap(~Sex, nrow=2) +
    theme_light() +
    theme(legend.position = "none") +
    geom_vline(xintercept = 2018.5) +
    geom_text(data=fore_text, aes(x = 2019, y = 5, label = label1), hjust=0) +
    geom_text(data=fore_text, aes(x = 2018, y = -30, label = label2), hjust=1) +
    labs(x="Calendar Year", y=NULL, title=expression("Period Effect ("*kappa["year"]*")")) + 
    scale_x_continuous(breaks=seq(2005, year, by=5)) 
  
  
  if (combine) {
    g.fit <- grid.arrange(g.ax, g.bx, g.kt, layout_matrix=matrix(c(1, 2, 3, 3), 2, 2, byrow = T))
    return(g.fit)
  } else {
    g.x <- grid.arrange(g.ax, g.bx, layout_matrix=matrix(c(1, 2), 1, 2, byrow = T))
    return(list(ax=g.ax, bx=g.bx, kt=g.kt, x=g.x))
  }
}

vis_age_trends_data <- function(fore, log_rate=T) {
  nms <- dimnames(fore$Female$Data)
  
  tr <- function(dd, name) {
    dimnames(dd) <- nms
    dd <- data.frame(as.table(dd))
    names(dd) <- c("Year", "Age", name)
    dd
  }
  
  dat <- rbind(
    with(fore$Female, {
      dat <- tr(Data * 1e5, "Data")
      dat$Sex <- "Female"
      dat
    }),
    with(fore$Male, {
      dat <- tr(Data * 1e5, "Data")
      dat$Sex <- "Male"
      dat
    })
  )
  
  g.dat <- ggplot(data=dat, aes(x=Year, colour=Age, fill=Age, group=Age)) +
    geom_point(aes(y=Data)) +
    geom_line(aes(y=Data)) + 
    facet_grid(.~Sex) +
    guides(fill=guide_legend(reverse=T), colour=guide_legend(reverse=T)) + 
    theme(legend.position="right", axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  
  if (log_rate) {
    g.dat <- g.dat + scale_y_log10("log(Incidence rate per 100 000)")
  } else {
    g.dat <- g.dat + scale_y_continuous("Incidence rate per 100 000")
  }
  g.dat
}


vis_age_trends <- function(fore, pars, log_rate=T) {
  nms <- dimnames(fore$Female$Data)
  
  tr <- function(dd, name) {
    dimnames(dd) <- nms
    dd <- data.frame(as.table(dd))
    names(dd) <- c("Year", "Age", name)
    dd
  }
  
  dat <- rbind(
    with(pars$Female, {
      dat <- tr(fore$Female$Data * 1e5, "Data")
      dat <- merge(dat, tr(fitted[,, 1] * 1e5, "Mean"))
      dat <- merge(dat, tr(fitted[,, 2] * 1e5, "Lower"))
      dat <- merge(dat, tr(fitted[,, 3] * 1e5, "Upper"))
      dat$Sex <- "Female"
      dat
    }),
    with(pars$Male, {
      dat <- tr(fore$Male$Data * 1e5, "Data")
      dat <- merge(dat, tr(fitted[,, 1] * 1e5, "Mean"))
      dat <- merge(dat, tr(fitted[,, 2] * 1e5, "Lower"))
      dat <- merge(dat, tr(fitted[,, 3] * 1e5, "Upper"))
      dat$Sex <- "Male"
      dat
    })
  )
  
  g.fit <- ggplot(data=dat, aes(x=Year, colour=Age, fill=Age, group=Age)) +
    geom_point(aes(y=Data)) +
    geom_line(aes(y=Mean)) + 
    geom_ribbon(aes(min=Lower, max=Upper), alpha=0.3) +
    facet_grid(.~Sex) +
    guides(fill=guide_legend(reverse=T), colour=guide_legend(reverse=T)) +
    theme(legend.position="right", axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  
  if (log_rate) {
    g.fit <- g.fit + scale_y_log10("log(Incidence rate per 100 000)")
  } else {
    g.fit <- g.fit + scale_y_continuous("Incidence rate per 100 000")
  }
  g.fit
}


vis_residuals <- function(ms, combine) {
  rd.f <- residuals(ms$Female)
  rd.f <- rd.f$residuals
  rownames(rd.f) <- ms$Age
  rd.f <- data.frame(as.table(rd.f))
  
  rd.m <- residuals(ms$Male)
  rd.m <- rd.m$residuals
  rownames(rd.m) <- ms$Age
  rd.m <- data.frame(as.table(rd.m))
  colnames(rd.f) <- colnames(rd.m) <- c("Age", "Year", "Residual")
  
  rd <- rbind(data.frame(rd.f, Sex="Female"), data.frame(rd.m, Sex="Male"))
  
  
  g.resid <- ggplot(data=rd, aes(x=Year, y=Age)) +
    geom_tile(aes(fill=Residual)) +
    facet_grid(~Sex) +
    scale_fill_gradientn(colours=c("blue","#FFFFFF","red")) +
    #  theme_classic() + 
    theme(legend.position="right", axis.text.x=element_text(angle=45, hjust=1, vjust=1))

  g.resid.year <- ggplot(data=rd, aes(x=Year, y=Residual)) + 
    geom_point(position = position_jitter()) +
    facet_grid(~Sex) +
    #  theme_minimal() + 
    theme(legend.position="right", axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  
  g.resid.age <- ggplot(data=rd, aes(x=Age, y=Residual)) + 
    geom_point(position = position_jitter()) +
    facet_grid(~Sex) +
    #  theme_minimal() + 
    theme(legend.position="right", axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  
  list(
    AgeYear=g.resid,
    Age=g.resid.age,
    Year=g.resid.year
  )
  
  if (combine) {
    g.fit <- grid.arrange(g.resid+theme(legend.position = "none"), 
                          g.resid.age+coord_flip(), 
                          g.resid.year, 
                          layout_matrix=matrix(c(1, 2, 3), 3, 1, byrow = T))
    return(g.fit)
  } else {
    return(list(
      AgeYear=g.resid,
      Age=g.resid.age,
      Year=g.resid.year
    ))
  }
}


vis_overall_trend <- function(summ, year, u=80) {
  dat <- summ$Data[c("Year", "TotalR")]
  dat <- subset(dat, Year <= year)
  ftt <- summ$Fitted[c("Year", "TotalR")]
  ftt <- subset(ftt, Year <= year)
  fore <- data.frame(Year=summ$Forecast$Year, summ$Boot[,"TotalR",])
  fore <- subset(fore, Year <= year)
  
  endTB <- data.frame(Year=c(2020, 2025, 2030, 2035),
                      Roles=c("Milestone", "Milestone", "Goal(SDG)", "Goal"),
                      Lab=c("20%", "50%", "80%", "90%"),
                      Noti=(1 - c(0.2, 0.5, 0.8, 0.9)) * 45.65)
  
  
  gnr <- ggplot(data=dat, aes(x=Year)) +
    geom_point(aes(y=TotalR, shape="Data")) +
    geom_line(aes(y=Mean, linetype="Forecast"), data=fore) +
    geom_ribbon(data=fore, aes(ymin=Lower, ymax=Upper), alpha=0.3) +
    geom_line(aes(y=TotalR, linetype="Fitted"), data=ftt) +
    geom_point(data=endTB, aes(y=Noti, shape="End TB Strategy"), size=2) +
    geom_point(aes(y=1, shape="Reduction"), size=0) +
    geom_text(data=endTB, aes(y=Noti, label=Lab, vjust=1.5, hjust=1)) +
    scale_linetype_manual(element_blank(), values=c("Fitted"=1, "Forecast"=2), drop=F) +
    scale_shape_manual(element_blank(), values=c(Data=16, "End TB Strategy"=2, "Reduction"=17), drop=F) +
    guides(linetype=guide_legend(order=1), shape=guide_legend(order=2), size="none") +
    scale_y_continuous("Incidence rate per 100 000", limits=c(0, u)) +
    scale_x_continuous("Year", breaks=seq(2005, year, by=5)) +
    theme_minimal() + 
    theme(legend.position="bottom")
  
  gnr
}


vis_age_reduction <- function(red, year=2035) {
  red <- red$Boot[as.character(year),,]
  
  red <- data.frame(
    Age=factor(rownames(red), levels=rownames(red)),
    red
  )
  
  red.all <- summary.red.age$All[as.character(year)]
  
  
  gre <- ggplot(data=red, aes(x=Age, y=Mean)) +
    geom_point(size=2, shape=17) + 
    geom_errorbar(aes(ymax=Lower, ymin=Upper), width=0.1) + 
    geom_hline(aes(yintercept=red.all), linetype="dashed") +
    geom_text(aes(x=8, y=red.all, label="Overall reduction"), hjust=0, vjust=2) +
    scale_y_continuous("Incidence rate reduction (%)", 
                       limits=c(min(red$Lower, 0), max(max(red$Upper), 1)),
                       labels = scales::percent) +
    theme_classic() +
    theme(legend.position="none", axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  
  gre
}


vis_age_share <- function(summ, year, agp, agl, u) {
  fore <- data.frame(Year=summ$Forecast$Year, summ$Boot[,endsWith(dimnames(summ$Boot)[[2]], "Fr"), 1])
  fore <- melt(fore, id.vars="Year", value.name="N")
  fore$Age <- agl[unlist(strsplit(as.character(fore$variable), "Fr"))]
  fore <- subset(fore, Year <= year)
  
  dat <- data.frame(Year=summ$Data$Year, summ$Data[,endsWith(dimnames(summ$Boot)[[2]], "Fr")])
  dat <- melt(dat, id.vars="Year", value.name="N")
  dat$Age <- agl[unlist(strsplit(as.character(dat$variable), "Fr"))]
  dat <- subset(dat, Year <= year)
  
  ftt <- data.frame(Year=summ$Fitted$Year, summ$Fitted[,endsWith(dimnames(summ$Boot)[[2]], "Fr")])
  ftt <- melt(ftt, id.vars="Year", value.name="N")
  ftt$Age <- agl[unlist(strsplit(as.character(ftt$variable), "Fr"))]
  ftt <- subset(ftt, Year <= year)
  
  
  gan <- ggplot(data=dat, aes(x=Year, y=N)) +
    geom_bar(aes(fill=Age), width=1, position=position_stack(reverse=TRUE), stat = "identity") +
    geom_bar(data=fore, stat = "identity", width=1, position=position_stack(reverse=TRUE), 
             aes(y=N, fill=Age, linetype="Forecast"), na.rm=T) +
    geom_vline(aes(xintercept=2018.4), linetype=2) + 
    geom_label(aes(x=2019, y=u*0.97, label="Forecast"), hjust=0, vjust=0.9) +
    geom_label(aes(x=2018, y=u*0.97, label="Data"), hjust=1, vjust=0.9) +
    scale_y_continuous("Incidence rate per 100 000", limits=c(0, u)) +
    scale_x_continuous("Year", breaks=seq(2005, year, by=5)) +
    scale_fill_grey("Age", start=0.8, end=0.3) +
    guides(fill=guide_legend(reverse=F), linetype="none") +
    theme_minimal()
  
  
  gan
  
}


vis_age_proportion <- function(summ, year, agp, agl) {
  fore <- data.frame(Year=summ$Forecast$Year, summ$Boot[,endsWith(dimnames(summ$Boot)[[2]], "Pr"), 1])
  fore <- melt(fore, id.vars="Year", value.name="N")
  fore$Age <- agl[unlist(strsplit(as.character(fore$variable), "Pr"))]
  fore <- subset(fore, Year <= year)
  
  dat <- data.frame(Year=summ$Data$Year, summ$Data[,endsWith(dimnames(summ$Boot)[[2]], "Pr")])
  dat <- melt(dat, id.vars="Year", value.name="N")
  dat$Age <- agl[unlist(strsplit(as.character(dat$variable), "Pr"))]
  dat <- subset(dat, Year <= year)
  
  ftt <- data.frame(Year=summ$Fitted$Year, summ$Fitted[,endsWith(dimnames(summ$Boot)[[2]], "Pr")])
  ftt <- melt(ftt, id.vars="Year", value.name="N")
  ftt$Age <- agl[unlist(strsplit(as.character(ftt$variable), "Pr"))]
  ftt <- subset(ftt, Year <= year)
  
  
  gan <- ggplot(data=dat, aes(x=Year, y=N)) +
    geom_bar(aes(fill=Age), width=1, position=position_stack(reverse=TRUE), stat = "identity") +
    geom_bar(data=fore, stat = "identity", width=1, position=position_stack(reverse=TRUE), 
             aes(y=N, fill=Age, linetype="Forecast"), na.rm=T) +
    geom_vline(aes(xintercept=2018.4), linetype=2) + 
    geom_label(aes(x=2019, y=0.97, label="Forecast"), hjust=0, vjust=0.9) +
    geom_label(aes(x=2018, y=0.97, label="Data"), hjust=1, vjust=0.9) +
    scale_y_continuous("Proportion (%)", limits=c(0, 1.02), labels = scales::percent) +
    scale_x_continuous("Year", breaks=seq(2005, year, by=5)) +
    scale_fill_grey("Age", start=0.8, end=0.3) +
    guides(fill=guide_legend(reverse=F), linetype="none") +
    theme_minimal()
  
  
  gan
}




vis_decomposition <- function(summ_b, summ_n, year) {
  dat <- summ_b$Data[, c("Year", "TotalR")]
  dat <- subset(dat, Year >= 2015)
  
  decom <- rbind(
    data.frame(
      Year=summ_b$Forecast$Year,
      Group="Forecast forward",
      summ_b$Boot[, "TotalR",]),
    data.frame(
      Year=summ_n$Forecast$Year,
      Group="Stop at 2018",
      summ_n$Boot[, "TotalR",])
  )
  decom <- subset(decom, Year <= year)
  
  
  endTB <- data.frame(Year=c(2020, 2025, 2030, 2035),
                      Roles=c("Milestone", "Milestone", "Goal(SDG)", "Goal"),
                      Lab=c("20%", "50%", "80%", "90%"),
                      Noti=(1 - c(0.2, 0.5, 0.8, 0.9)) * 45.65
  )
  
  gdec <- ggplot(data=decom, aes(x=Year)) +
    geom_line(aes(y=Mean, colour=Group)) +
    geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=Group), alpha=0.3) +
    geom_point(data=dat, aes(y=TotalR, shape="Data")) +
    geom_point(data=endTB, aes(y=Noti, shape="End TB Strategy"), size=2) +
    geom_text(data=endTB, aes(y=Noti, label=Lab, vjust=1.5, hjust=1)) +
    scale_y_continuous("Incidence rate per 100 000", limits=c(0, 60)) +
    scale_colour_discrete("Demographic change") +
    scale_fill_discrete("Demographic change") + 
    scale_shape_manual(element_blank(), values=c(Data=16, "End TB Strategy"=2)) +
    theme_minimal()
  
  gdec
}
