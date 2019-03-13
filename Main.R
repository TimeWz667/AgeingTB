rm(list=ls())
library(demography)
library(StMoMo)
library(ggplot2)

source("Source/estimate.R")
source("Source/simulate.R")
source("Source/extract.R")

load("E:/Source/AgeingTB/Input/Incidence.rdata")

load("E:/Source/AgeingTB/Input/Population_1000.rdata")
# load("E:/Source/AgeingTB/Input/Population_10000.rdata")

n.sim <- length(pop.f$Boot)

models <- estimate(inc.f, inc.m, pop.f, pop.m)


set.seed(1166)

forecasts <- forecast_rates(models, 2050, n.sim)





# This step is time-consuming
n.sim=200
parameters <- extract_parameters(models, n_boot=n.sim)
kts <- extract_forecast_kt(forecasts)

