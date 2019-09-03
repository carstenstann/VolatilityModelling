#########################################################################################
## Project: VolatilityModelling
## Script purpose: fit ARCH models to data
## Date: 01.09.2019
## Author: Carsten Stann
#########################################################################################

library(rugarch)

# run import and clean script ----------------------------------------------------------
source("./R/01_import_and_clean.R") # included packages: quantmod, tidyverse, timetk, xts

# inspect cleaned data 
returns

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
library(tictoc)
tic()
roll <- returns %>%
   filter(market_index == "GSPC.Adjusted") %>% 
   mutate(garch_1_1 = map(train, ~ugarchroll(spec = garch_1_1_spec, 
                                             data = ., 
                                             n.ahead = 1,
                                             n.start = 1000,
                                             refit.every = 25, # refit every 4 weeks
                                             refit.window = "moving",
                                             window.size = 1000)),
          tarch_1_roll = map(train, ~ugarchroll(spec = tarch_specs[[1]], 
                                                data = ., 
                                                n.ahead = 1,
                                                n.start = 1000,
                                                refit.every = 25, # refit every 4 weeks
                                                refit.window = "moving",
                                                window.size = 1000)),
          tgarch_1_1_roll = map(train, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                data = ., 
                                                n.ahead = 1,
                                                n.start = 1000,
                                                refit.every = 25, # refit every 4 weeks
                                                refit.window = "moving",
                                                window.size = 1000)))
toc()

# roll_forecast$arch_1_roll[[1]] %>% plot()

# report(roll_forecast$arch_1_roll[[1]], type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)
