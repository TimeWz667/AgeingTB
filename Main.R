rm(list=ls())
library(demography)
library(StMoMo)
library(ggplot2)


load("E:/Source/AgeingTB/Input/Incidence.rdata")

load("E:/Source/AgeingTB/Input/Population_1000.rdata")
# load("E:/Source/AgeingTB/Input/Population_10000.rdata")

ages <- 1:15
years <- as.integer(rownames(inc.f))

x.f <- demogdata(t(inc.f/pop.f$Data), t(pop.f$Data), ages=ages, years=years, 
                   type="mortality", label="Taiwan", name="Female")
x.f <- StMoMoData(x.f)


x.m <- demogdata(t(inc.m/pop.m$Data), t(pop.m$Data), ages=ages, years=years, 
                 type="mortality", label="Taiwan", name="Male")
x.m <- StMoMoData(x.m)

