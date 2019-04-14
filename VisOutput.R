library(ggplot2)
library(gridExtra)
library(ggpubr)

load("Output/results1000.rdata")
load("Input/Population_10000.rdata")


agp <- list(Chd=1:3, Young=4:7, Mid=8:13, Old=14:15)
agl <- c(Chd="0-14", Young="15-34", Mid="35-64", Old="65+")


year.end <- 2035

# Data
g.data <- vis_age_trends_data(forecasts, log_rate=T)
ggsave("Output/DataTrend.jpg", width = 20, height = 12, units = "cm", plot=g.data)

g.as <- vis_age_struture(pop.f, pop.m, year.end, agp, agl)


# Modelling
g.pars <- vis_parameters(parameters, kts, combine=T)
ggsave("Output/Fit.jpg", width = 15, height = 20, units="cm", plot=g.pars)


g.pars.full <- vis_parameters(parameters, kts, combine=F)
ggsave("Output/Fit_x.jpg", width = 15, height = 10, units = "cm", plot=g.pars.full$x)
ggsave("Output/Fit_t.jpg", width = 15, height = 10, units = "cm", plot=g.pars.full$kt)


g.fit <- vis_age_trends(forecasts, parameters, log_rate=T)
ggsave("Output/Fitted.jpg", width = 20, height = 12, units = "cm", plot=g.fit)

g.resid <- vis_residuals(models, combine=T)
ggsave("Output/Residuals.jpg", width = 15, height = 20, units="cm", plot=g.resid)

g.resid <- vis_residuals(models, combine=F)
g.resid.at <- grid.arrange(g.resid$Age, 
                           g.resid$Year, 
                      layout_matrix=matrix(c(1, 2), 2, 1, byrow = T))

ggsave("Output/Residuals_xt.jpg", width = 15, height = 8, units="cm", plot=g.resid$AgeYear)
ggsave("Output/Residuals_x_t.jpg", width = 15, height = 15, units="cm", plot=g.resid.at)

# Forecasting
g.trend <- vis_overall_trend(summary.age, year.end, 80)
ggsave("Output/ForecastTrend.jpg", width = 15, height = 10, units = "cm", plot=g.trend)

g.share <- vis_age_share(summary.age, year.end, agp, agl, 80)
ggsave("Output/ForecastShare.jpg", width = 15, height = 10, units = "cm", plot=g.share)

g.prop <- vis_age_proportion(summary.age, year.end, agp, agl)
ggsave("Output/ForecastProp.jpg", width = 15, height = 10, units = "cm", plot=g.prop)

g.red <- vis_age_reduction(summary.red.age, year.end)
ggsave("Output/ForecastReduction.jpg", width = 15, height = 10, units = "cm", plot=g.red)


g.fore.upper <- ggarrange(g.trend + labs(tag="A", title="Incidence rate: fitting and forecasting"), 
                          g.red + labs(tag="B", title=paste0("Incidence rate reduction (%) during 2015-", year.end)), 
                          ncol=2, common.legend=TRUE, legend="bottom")
g.fore.lower <- ggarrange(g.share + labs(tag="C", title="Incidence rate by age"), 
                          g.prop + labs(tag="D", title="Incidence composition"), 
                          ncol=2, common.legend=TRUE, legend="bottom")
g.fore <- ggarrange(g.fore.upper, g.fore.lower, nrow=2, heights=c(10, 8))
ggsave("Output/Forecast.jpg", width = 25, height = 20, units = "cm", plot=g.fore)

# Scenario analysis
g.dec <- vis_decomposition(summary.age, summary.age.fixed, year.end)
ggsave("Output/DemographicEffect.jpg", width = 15, height = 10, units = "cm", plot=g.dec)
