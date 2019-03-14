library(ggplot2)
library(gridExtra)
library(ggpubr)

# Data
g.data <- vis_age_trends_data(forecasts, log_rate=T)
ggsave("Output/DataTrend.jpg", width = 20, height = 12, units = "cm", plot=g.data)

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


# Forecasting
g.trend <- vis_overall_trend(summary.age, 2035, 80)
ggsave("Output/ForecastTrend.jpg", width = 15, height = 10, units = "cm", plot=g.trend)

g.share <- vis_age_share(summary.age, 2035, agp, agl, 80)
ggsave("Output/ForecastShare.jpg", width = 15, height = 10, units = "cm", plot=g.share)

g.prop <- vis_age_proportion(summary.age, 2035, agp, agl)
ggsave("Output/ForecastProp.jpg", width = 15, height = 10, units = "cm", plot=g.prop)

g.red <- vis_age_reduction(summary.red.age, 2035)
ggsave("Output/ForecastReduction.jpg", width = 15, height = 10, units = "cm", plot=g.red)


g.fore.upper <- ggarrange(g.trend + labs(tag="A", title="Incidence rate: fitting and forecasting"), 
                          g.red + labs(tag="B", title="Incidence rate reduction (%) during 2015-2035"), 
                          ncol=2, common.legend=TRUE, legend="bottom")
g.fore.lower <- ggarrange(g.share + labs(tag="C", title="Incidence rate by age"), 
                          g.prop + labs(tag="D", title="Incidence composition"), 
                          ncol=2, common.legend=TRUE, legend="bottom")
g.fore <- ggarrange(g.fore.upper, g.fore.lower, nrow=2, heights=c(10, 8))
ggsave("Output/Forecast.jpg", width = 25, height = 20, units = "cm", plot=g.fore)

# Scenario analysis
g.dec <- vis_decomposition(summary.age, summary.age.fixed, 2035)
ggsave("Output/DemographicEffect.jpg", width = 15, height = 10, units = "cm", plot=g.dec)






