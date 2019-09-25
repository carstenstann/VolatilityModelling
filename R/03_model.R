#########################################################################################
## Project: VolatilityModelling
## Script purpose: fit ARCH models to data
## Date: 01.09.2019
## Author: Carsten Stann
#########################################################################################

library(rugarch)
library(tictoc)
library(scales)

# run import and clean script ----------------------------------------------------------
source("./R/01_import_and_clean.R") # included packages: quantmod, tidyverse, timetk, xts

# inspect cleaned data 
returns
nested_returns

# model specifications ------------------------------------------------------------------

# GARCH(1,1)
garch_1_1_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(0,0), 
                                               include.mean = TRUE),
                             distribution.model = "std")

# TGARCH(1,1) TGARCH Zakoian (1994)
tgarch_1_1_spec <- ugarchspec(variance.model = list(model = "fGARCH", 
                                                    garchOrder = c(1,0),
                                                    submodel = "TGARCH"),
                              mean.model = list(armaOrder = c(0,0),
                                                include.mean = TRUE),
                              fixed.pars = list(delta = 1),
                              distribution.model = "std")

# GJRGARCH(1,1) Glosten et al.(1993)
gjrgarch_1_1_spec <- ugarchspec(variance.model = list(model = "gjrGARCH",
                                                      garchOrder = c(1,1)),
                                mean.model = list(armaOrder = c(0,0)),
                                distribution.model = "std")

# EGARCH Nelson (1991)
egarch_1_1_spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                                    garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,0)),
                              distribution.model = "std")

# rolling density forecast --------------------------------------------------------------

roll_returns <- nested_returns %>%
   mutate(data_xts = map(data, ~tk_xts(., select = return, date_var = date)))

tic()
garch_roll <- roll_returns %>% 
   mutate(garch_1_1_n1000 = map(data_xts, ~ugarchroll(spec = garch_1_1_spec, 
                                                      data = ., 
                                                      forecast.length = 252,
                                                      refit.every = 22, # refit every 4 weeks
                                                      refit.window = "moving",
                                                      window.size = 1000)),
          garch_1_1_n2000 = map(data_xts, ~ugarchroll(spec = garch_1_1_spec, 
                                                      data = ., 
                                                      forecast.length = 252,
                                                      refit.every = 22, # refit every 4 weeks
                                                      refit.window = "moving",
                                                      window.size = 2000)),
          garch_1_1_n4000 = map(data_xts, ~ugarchroll(spec = garch_1_1_spec, 
                                                      data = ., 
                                                      forecast.length = 252,
                                                      refit.every = 22, # refit every 4 weeks
                                                      refit.window = "moving",
                                                      window.size = 4000)))
toc()

tic()
gjrgarch_roll <- roll_returns %>% 
   mutate(gjrgarch_1_1_n1000 = map(data_xts, ~ugarchroll(spec = gjrgarch_1_1_spec,
                                                         data = .,
                                                         forecast.length = 252,
                                                         refit.every = 22,
                                                         refit.window = "moving",
                                                         window.size = 1000)),
          gjrgarch_1_1_n2000 = map(data_xts, ~ugarchroll(spec = gjrgarch_1_1_spec,
                                                         data = .,
                                                         forecast.length = 252,
                                                         refit.every = 22,
                                                         refit.window = "moving",
                                                         window.size = 2000)),
          gjrgarch_1_1_n4000 = map(data_xts, ~ugarchroll(spec = gjrgarch_1_1_spec,
                                                         data = .,
                                                         forecast.length = 252,
                                                         refit.every = 22,
                                                         refit.window = "moving",
                                                         window.size = 4000)))
toc()

tic()
egarch_roll <- roll_returns %>% 
   mutate(egarch_1_1_n1000 = map(data_xts, ~ugarchroll(spec = egarch_1_1_spec,
                                                       data = .,
                                                       forecast.length = 252,
                                                       refit.every = 22,
                                                       refit.window = "moving",
                                                       window.size = 1000)),
          egarch_1_1_n2000 = map(data_xts, ~ugarchroll(spec = egarch_1_1_spec,
                                                       data = .,
                                                       forecast.length = 252,
                                                       refit.every = 22,
                                                       refit.window = "moving",
                                                       window.size = 2000)),
          egarch_1_1_n4000 = map(data_xts, ~ugarchroll(spec = egarch_1_1_spec,
                                                       data = .,
                                                       forecast.length = 252,
                                                       refit.every = 22,
                                                       refit.window = "moving",
                                                       window.size = 4000)))
toc()

tic()
tgarch_roll <- roll_returns %>% 
   mutate(tgarch_1_1_n1000 = map(data_xts, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                       data = ., 
                                                       forecast.length = 252,
                                                       refit.every = 22, # refit every 4 weeks
                                                       refit.window = "moving",
                                                       window.size = 1000)),
          tgarch_1_1_n2000 = map(data_xts, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                       data = ., 
                                                       forecast.length = 252,
                                                       refit.every = 22, # refit every 4 weeks
                                                       refit.window = "moving",
                                                       window.size = 2000)),
          tgarch_1_1_n4000 = map(data_xts, ~ugarchroll(spec = tgarch_1_1_spec, 
                                                       data = ., 
                                                       forecast.length = 252,
                                                       refit.every = 22, # refit every 4 weeks
                                                       refit.window = "moving",
                                                       window.size = 4000)))
toc()

# bind models together
rolling_forecasts <- bind_cols(#arch_roll, 
                               garch_roll,
                               #tarch_roll[, -c(1:3)],
                               tgarch_roll[, -c(1:3)],
                               gjrgarch_roll[, -c(1:3)],
                               egarch_roll[,-c(1:3)])

# Compute the 5% value at risk
normgarchVaR <- quantile(normgarchroll, probs = 0.05)
sstdgarchVaR <- quantile(sstdgarchroll, probs = 0.05)

# Compute the coverage
actual <- xts(as.data.frame(normgarchroll)$Realized,time(normgarchVaR))
mean(actual < normgarchVaR)
mean(actual < sstdgarchVaR)


# visualize forecasts -------------------------------------------------------------------

rolling_forecasts %>% 
   gather(model, forecast, -c(market_index, data, data_xts)) %>% 
   separate(model, into = c("model", "sample_size"), sep = "_n") %>% 
   mutate(sample_size = parse_number(sample_size),
          report = map(forecast, ~report(., VaR.alpha = 0.05)@forecast$VaR),
          report = map(report, ~rownames_to_column(., var = "date"))) %>% 
   select(market_index, model, sample_size, report) %>% 
   unnest(cols = report) %>% 
   mutate(date = parse_date(date, format = "%Y-%m-%d")) %>% 
   filter(sample_size == 1000) %>% 
   #filter(model %in% c("garch_1_1")) %>% 
ggplot() +
   geom_line(aes(x = date, y = realized), alpha = 0.5) +
   geom_line(aes(x = date, y = `alpha(1%)`, col = factor(sample_size), group = factor(sample_size))) +
   theme_minimal() +
   facet_wrap(~model, ncol = 4) +
   labs(x = NULL)
   
# VaR forecast plots --------------------------------------------------------------------
rolling_forecasts %>% 
   gather(model, forecast, -c(market_index, data, data_xts)) %>% 
   separate(model, into = c("model", "sample_size"), sep = "_n") %>% 
   select(-data, -data_xts) %>% 
   mutate(sample_size = parse_number(sample_size),
          report = map(forecast, ~report(., VaR.alpha = 0.05)@forecast$VaR),
          report = map(report, ~rownames_to_column(., var = "date"))) %>% 
   unnest(cols = report) %>% 
   ungroup() %>% 
   mutate(date = parse_date(date, format = "%Y-%m-%d")) %>% 
   filter(sample_size == 4000) %>% 
   mutate(model = str_replace(model, "_", " "), 
          market_index = str_replace(market_index, ".Adjusted", "")) %>% 
ggplot() +
   geom_line(aes(x = date, y = realized), alpha = 0.5) +
   geom_line(aes(x = date, y = `alpha(1%)`), col = "blue", alpha = 0.7) +
   theme_minimal() +
   facet_grid(model ~ market_index, scale = "free_y") +
   scale_x_date(label = date_format("%b-%y"),
                date_breaks = "3 months") +
   labs(title = "One Day Ahead Value-at-Risk Forecasts by Market Index and Model Specification",
        x = NULL,
        y = "Daily log-return")


# model averaging
variance.models <- c("sGARCH", "gjrGARCH")
distribution.models <- c("norm", "std", "std")
c <- 1
for (variance.model in variance.models) {
   for (distribution.model in distribution.models) {
      garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                              variance.model = list(model = variance.model),
                              distribution.model = distribution.model)
      garchfit <- ugarchfit(data = msftret, spec = garchspec)
      if (c==1) {
         msigma <- sigma(garchfit)
      } else {
         msigma <- merge(msigma, sigma(garchfit))
      } 
      c <- c + 1
   }
}


GARCH covariance

msftwmtcov <- msftwmtcor * sigma(msftgarchfit) * sigma(wmtgarchfit)


