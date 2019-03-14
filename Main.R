rm(list=ls())
library(demography)
library(StMoMo)
library(ggplot2)

source("Source/estimate.R")
source("Source/simulate.R")
source("Source/summarise.R")
source("Source/extract.R")
source("Source/visualise.R")



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



agp <- list(Chd=1:3, Young=4:7, Mid=8:13, Old=14:15)
agl <- c(Chd="0-14", Young="15-34", Mid="35-64", Old="65+")

summary.age <- summarise_all_age(sims, agp)

vis_age_trends_data(forecasts, log_rate=T)
vis_overall_trend(summary.age, 2035, 80)

vis_age_share(summary.age, 2035, agp, agl, 80)

vis_age_proportion(summary.age, 2035, agp, agl)



summary.red.age <- summarise_reduction_age(sims, 2015) 

vis_age_reduction(summary.red.age, 2035)



summary.red.all <- summarise_reduction_all(sims, 2015)




sims.fixed <- simulate_incidence_fix_demo(forecasts, pop.f, pop.m)

summary.age.fixed <- summarise_all_age(sims.fixed, agp)
summary.red.all.fixed <- summarise_reduction_all(sims.fixed, 2015)

vis_decomposition(summary.age, summary.age.fixed, 2035)



# This step is time-consuming
n.sim=200 # comment this line for a full bootstrappiing
parameters <- extract_parameters(models, n_boot=n.sim)
kts <- extract_forecast_kt(forecasts)

vis_parameters(parameters, kts, combine=T)

vis_residuals(models, combine=T)



vis_age_trends(forecasts, parameters, log_rate=T)
