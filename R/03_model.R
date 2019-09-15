#########################################################################################
## Project: VolatilityModelling
## Script purpose: fit ARCH models to data
## Date: 01.09.2019
## Author: Carsten Stann
#########################################################################################

library(rugarch)
library(tictoc)

# run import and clean script ----------------------------------------------------------
source("./R/01_import_and_clean.R") # included packages: quantmod, tidyverse, timetk, xts

# inspect cleaned data 
returns
nested_returns

# model specifications ------------------------------------------------------------------
# Arch(1:5)
arch_specs <- map(1:5, ~ugarchspec(variance.model = list(garchOrder = c(.,0)),
                                      mean.model = list(armaOrder = c(0,0),
                                                        include.mean = FALSE)))
# TARCH(1:5) TGARCH Zakoian (1994)
tarch_specs <- map(1:5, ~ugarchspec(variance.model = list(model = "apARCH", 
                                                              garchOrder = c(.,0),
                                                              submodel = "TGARCH"),
                                        mean.model = list(armaOrder = c(0,0), 
                                                          include.mean = FALSE),
                                        fixed.pars = list(delta = 1)))
# GARCH(1,1)
garch_1_1_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,0), 
                                               include.mean = FALSE))

# TGARCH(1,1) TGARCH Zakoian (1994)
tgarch_1_1_spec <- ugarchspec(variance.model = list(model = "apARCH", 
                                                    garchOrder = c(1,0),
                                                    submodel = "TGARCH"),
                              mean.model = list(armaOrder = c(0,0),
                                                include.mean = FALSE),
                              fixed.pars = list(delta = 1))

# rolling density forecast --------------------------------------------------------------

tic()
roll <- nested_returns %>%
   filter(market_index == "GSPC.Adjusted") %>% 
   mutate(data_xts = map(data, ~tk_xts(., select = return, date_var = date))) %>% 
   mutate(garch_1_1_n1000 = map(data_xts, ~ugarchroll(spec = garch_1_1_spec, 
                                             data = ., 
                                             forecast.length = 365,
                                             refit.every = 25, # refit every 4 weeks
                                             refit.window = "moving",
                                             window.size = 1000)),
          arch_1_n1000 = map(data_xts, ~ugarchroll(spec = arch_specs[[1]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 1000)),
          arch_2_n1000 = map(data_xts, ~ugarchroll(spec = arch_specs[[2]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 1000)),
          arch_3_n1000 = map(data_xts, ~ugarchroll(spec = arch_specs[[3]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 1000)),
          tarch_1_n1000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[1]], 
                                                data = ., 
                                                forecast.length = 365,
                                                refit.every = 25, # refit every 4 weeks
                                                refit.window = "moving",
                                                window.size = 1000)),
          tarch_2_n1000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[2]], 
                                                   data = ., 
                                                   forecast.length = 365,
                                                   refit.every = 25, # refit every 4 weeks
                                                   refit.window = "moving",
                                                   window.size = 1000)),
          tarch_3_n1000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[3]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 1000)),
          tgarch_1_1_n1000 = map(data_xts, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                data = ., 
                                                forecast.length = 365,
                                                refit.every = 25, # refit every 4 weeks
                                                refit.window = "moving",
                                                window.size = 1000))
   ) %>% 
   mutate(garch_1_1_n2000 = map(data_xts, ~ugarchroll(spec = garch_1_1_spec, 
                                                data = ., 
                                                forecast.length = 365,
                                                refit.every = 25, # refit every 4 weeks
                                                refit.window = "moving",
                                                window.size = 2000)),
          arch_1_n2000 = map(data_xts, ~ugarchroll(spec = arch_specs[[1]], 
                                             data = ., 
                                             forecast.length = 365,
                                             refit.every = 25, # refit every 4 weeks
                                             refit.window = "moving",
                                             window.size = 2000)),
          arch_2_n2000 = map(data_xts, ~ugarchroll(spec = arch_specs[[2]], 
                                             data = ., 
                                             forecast.length = 365,
                                             refit.every = 25, # refit every 4 weeks
                                             refit.window = "moving",
                                             window.size = 2000)),
          arch_3_n2000 = map(data_xts, ~ugarchroll(spec = arch_specs[[3]], 
                                             data = ., 
                                             forecast.length = 365,
                                             refit.every = 25, # refit every 4 weeks
                                             refit.window = "moving",
                                             window.size = 2000)),
          tarch_1_n2000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[1]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 2000)),
          tarch_2_n2000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[2]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 2000)),
          tarch_3_n2000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[3]], 
                                              data = ., 
                                              forecast.length = 365,
                                              refit.every = 25, # refit every 4 weeks
                                              refit.window = "moving",
                                              window.size = 2000)),
          tgarch_1_1_n2000 = map(data_xts, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                 data = ., 
                                                 forecast.length = 365,
                                                 refit.every = 25, # refit every 4 weeks
                                                 refit.window = "moving",
                                                 window.size = 2000))
   ) %>% 
   mutate(garch_1_1_n4000 = map(data_xts, ~ugarchroll(spec = garch_1_1_spec, 
                                                      data = ., 
                                                      forecast.length = 365,
                                                      refit.every = 25, # refit every 4 weeks
                                                      refit.window = "moving",
                                                      window.size = 4000)),
          arch_1_n4000 = map(data_xts, ~ugarchroll(spec = arch_specs[[1]], 
                                                   data = ., 
                                                   forecast.length = 365,
                                                   refit.every = 25, # refit every 4 weeks
                                                   refit.window = "moving",
                                                   window.size = 4000)),
          arch_2_n4000 = map(data_xts, ~ugarchroll(spec = arch_specs[[2]], 
                                                   data = ., 
                                                   forecast.length = 365,
                                                   refit.every = 25, # refit every 4 weeks
                                                   refit.window = "moving",
                                                   window.size = 4000)),
          arch_3_n4000 = map(data_xts, ~ugarchroll(spec = arch_specs[[3]], 
                                                   data = ., 
                                                   forecast.length = 365,
                                                   refit.every = 25, # refit every 4 weeks
                                                   refit.window = "moving",
                                                   window.size = 4000)),
          tarch_1_n4000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[1]], 
                                                    data = ., 
                                                    forecast.length = 365,
                                                    refit.every = 25, # refit every 4 weeks
                                                    refit.window = "moving",
                                                    window.size = 4000)),
          tarch_2_n4000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[2]], 
                                                    data = ., 
                                                    forecast.length = 365,
                                                    refit.every = 25, # refit every 4 weeks
                                                    refit.window = "moving",
                                                    window.size = 4000)),
          tarch_3_n4000 = map(data_xts, ~ugarchroll(spec = tarch_specs[[3]], 
                                                    data = ., 
                                                    forecast.length = 365,
                                                    refit.every = 25, # refit every 4 weeks
                                                    refit.window = "moving",
                                                    window.size = 4000)),
          tgarch_1_1_n4000 = map(data_xts, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                       data = ., 
                                                       forecast.length = 365,
                                                       refit.every = 25, # refit every 4 weeks
                                                       refit.window = "moving",
                                                       window.size = 4000)))
toc()

# clean rolling forecasts

roll %>% 
   gather(model, forecast, -c(market_index, data, data_xts)) %>% 
   separate(model, into = c("model", "sample_size"), sep = "_n") %>% 
   mutate(sample_size = parse_number(sample_size)) %>% 
   mutate(report = map(forecast, ~report(., VaR.alpha = 0.05)@forecast$VaR),
          report = map(report, ~rownames_to_column(., var = "date"))) %>% 
   select(market_index, model, sample_size, report) %>% 
   unnest() %>% 
   mutate(date = parse_date(date, format = "%Y-%m-%d")) %>% 
   #filter(model %in% c("garch_1_1")) %>% 
ggplot() +
   geom_line(aes(x = date, y = realized), alpha = 0.5) +
   geom_line(aes(x = date, y = `alpha(1%)`, col = factor(sample_size), group = factor((sample_size)))) +
   theme_minimal() +
   facet_wrap(~model)
   



roll$report[[1]]@forecast$VaR %>% str()

report(roll$forecast[[1]])@forecast$VaR


roll$garch_1_1_n2000[[1]] %>% plot() #report(VaR.alpha = 0.05)


roll$garch_1_1[[3]] %>% report()

roll$garch_1_1[[3]]@forecast$VaR %>% 
   summarise(sum(realized < `alpha(1%)`))

# report(roll_forecast$arch_1_roll[[1]], type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)


























