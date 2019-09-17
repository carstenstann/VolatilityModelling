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
                                                        include.mean = TRUE)))
# TARCH(1:5) TGARCH Zakoian (1994)
tarch_specs <- map(1:5, ~ugarchspec(variance.model = list(model = "apARCH", 
                                                              garchOrder = c(.,0),
                                                              submodel = "TGARCH"),
                                        mean.model = list(armaOrder = c(0,0), 
                                                          include.mean = TRUE),
                                        fixed.pars = list(delta = 1)))
# GARCH(1,1)
garch_1_1_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,0), 
                                               include.mean = TRUE))

# TGARCH(1,1) TGARCH Zakoian (1994)
tgarch_1_1_spec <- ugarchspec(variance.model = list(model = "apARCH", 
                                                    garchOrder = c(1,0),
                                                    submodel = "TGARCH"),
                              mean.model = list(armaOrder = c(0,0),
                                                include.mean = TRUE),
                              fixed.pars = list(delta = 1))

# rolling density forecast --------------------------------------------------------------

tic()
roll <- nested_returns %>%
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
   unnest(cols = report) %>% 
   mutate(date = parse_date(date, format = "%Y-%m-%d")) %>% 
   filter(sample_size == 2000) %>% 
   #filter(model %in% c("garch_1_1")) %>% 
ggplot() +
   geom_line(aes(x = date, y = realized), alpha = 0.5) +
   geom_line(aes(x = date, y = `alpha(1%)`, col = factor(sample_size), group = factor(sample_size))) +
   theme_minimal() +
   facet_wrap(~model, ncol = 4) +
   labs(x = NULL)
   

roll %>% 
   gather(model, forecast, -c(market_index, data, data_xts)) %>% 
   separate(model, into = c("model", "sample_size"), sep = "_n") %>% 
   select(-data, -data_xts) %>% 
   mutate(sample_size = parse_number(sample_size),
          report = map(forecast, ~report(., VaR.alpha = 0.05)@forecast$VaR),
          report = map(report, ~rownames_to_column(., var = "date"))) %>% 
   unnest(cols = report) %>% 
   ungroup() %>% 
   mutate(date = parse_date(date, format = "%Y-%m-%d")) %>% 
   filter(model %in%  c("arch_3", "tarch_3", "garch_1_1", "tgarch_1_1"), sample_size == 4000) %>% 
   mutate(model = str_replace(model, "_", " "), 
          market_index = str_replace(market_index, ".Adjusted", "")) %>% 
ggplot() +
   geom_line(aes(x = date, y = realized), alpha = 0.5) +
   geom_line(aes(x = date, y = `alpha(1%)`), col = "blue", alpha = 0.7) +
   theme_minimal() +
   facet_grid(model ~ market_index) +
   labs(title = "One Day Ahead Value-at-Risk Forecasts by Market Index and Model Specification",
        x = NULL,
        y = "Daily log-return")

