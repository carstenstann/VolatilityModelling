#########################################################################################
## Project: VolatilityModelling
## Script purpose: import equity index data
## Date: 03.09.2019
## Author: Carsten Stann
#########################################################################################

library(quantmod)
library(tidyverse)
library(rugarch)
library(timetk)

getSymbols(Symbols = c("^GSPC", "^DJI", "^FTSE", "^GDAXI","^FCHI", "^N225"), 
              src = "yahoo", 
              from = "2000-01-01",
              auto.assign = TRUE)

index <- Ad(cbind(GSPC, DJI, FTSE, GDAXI, FCHI, N225)) %>% 
   na.approx(na.rm = FALSE) %>% 
   na.omit()

returns <- diff(log(index))[-1,]["2000/"] %>% 
   tk_tbl(rename_index = "date") %>% 
   gather(market_index, return, -date) %>% 
   group_by(market_index) %>% 
   nest() %>% 
   mutate(train = map(data, ~tk_xts(., select = return, date_var = date)))
   
# model specifications ------------------------------------------------------------------
arch_p <- 1:5
tarch_t <- 1:5

# Arch(1:5)
arch_specs <- map(arch_p, ~ugarchspec(variance.model = list(garchOrder = c(.,0)),
                                      mean.model = list(armaOrder = c(0,0),
                                                        include.mean = FALSE)))
# TARCH(1:5) TGARCH Zakoian (1994)
tarch_specs <- map(tarch_t, ~ugarchspec(variance.model = list(model = "apARCH", 
                                                              garchOrder = c(.,0),
                                                              submodel = "TGARCH"),
                                        mean.model = list(armaOrder = c(0,0), 
                                                          include.mean = FALSE),
                                        fixed.pars = list(delta = 1)))
# GARCH(1,1)
garch_1_1_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,0), 
                                               include.mean = FALSE))

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
                                                window.size = 1000)))
toc()

roll$garch_1_1[[1]] %>% plot()

report(roll$garch_1_1[[1]], type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)

ugarchfit(spec = tarch_specs[[1]],
          data = returns$train[[1]], 
          solver = "hybrid")





