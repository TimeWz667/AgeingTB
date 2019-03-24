rm(list=ls())
library(demography)
library(StMoMo)
library(ggplot2)

source("Source/estimate.R")
source("Source/simulate.R")
source("Source/summarise.R")
source("Source/extract.R")
source("Source/visualise.R")


# Setting Age groups
agp <- list(Chd=1:3, Young=4:7, Mid=8:13, Old=14:15)
agl <- c(Chd="0-14", Young="15-34", Mid="35-64", Old="65+")

load("Input/Incidence.rdata")
load("Input/Population_1000.rdata")
# load("Input/Population_10000.rdata")

n.sim <- length(pop.f$Boot)

models <- estimate(inc.f, inc.m, pop.f, pop.m)


set.seed(1166)

# Forecast future incidence rates
forecasts <- forecast_rates(models, 2050, n.sim, order_f=c(0, 1, 0), order_m=c(0, 1, 0))


# Sample future incidence cases based on rates and population sizes
sims <- simulate_incidence(forecasts, pop.f, pop.m)

summary.age <- summarise_all_age(sims, agp)

summary.red.age <- summarise_reduction_age(sims, 2015) 

summary.red.all <- summarise_reduction_all(sims, 2015)



sims.fixed <- simulate_incidence_fix_demo(forecasts, pop.f, pop.m)

summary.age.fixed <- summarise_all_age(sims.fixed, agp)

summary.red.all.fixed <- summarise_reduction_all(sims.fixed, 2015)

summary.paf <- summarise_paf(sims1=sims, sims0=sims.fixed)


# This step is time-consuming
# 
parameters <- extract_parameters(models, n_boot=n.sim)
kts <- extract_forecast_kt(forecasts)


save(parameters, kts, forecasts,
     sims, summary.age, summary.red.age, summary.red.all, 
     sims.fixed, summary.age.fixed, summary.red.all.fixed, summary.paf,
     file="Output/results1000.rdata")

