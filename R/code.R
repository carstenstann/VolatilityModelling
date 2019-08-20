library(tidyverse)
library(timetk)
library(lubridate)
library(xts)
library(forecast)
library(tsibble)
library(cowplot)

# Import data ---------------------------------------------------------------------------

import <- read_csv("Data/OxfordManRealizedVolatilityIndices.csv", skip = 2)

# clean data ----------------------------------------------------------------------------

vol_data <- import %>%
   mutate(
      date = parse_date(as.character(DateID),format = "%Y%m%d")
   ) %>% 
   select(
      date, 
      contains(".r"), 
      -contains(".rk")
   ) %>% 
   select(
      date, 
      SP500.r = SPX2.r,
      SP500.rv = SPX2.rv,
      DJI.r = DJI2.r,
      DJI.rv = DJI2.rv,
      FTSE.r = FTSE2.r,
      FTSE.rv = FTSE2.rv,
      DAX.r = GDAXI2.r,
      DAX.rv = GDAXI2.rv,
      CAC.r = FCHI2.r,
      CAC.rv = FCHI2.rv,
      Nikkei.r = N2252.r,
      Nikkei.rv= N2252.rv
   )

# interpolate NAs and convert to tsibble for modelling

vol_tsibble <- tk_xts(data = vol_data, silent = TRUE) %>% 
   # interpolate NAs
   na.approx(na.rm = FALSE) %>% 
   tk_tbl(rename_index = "date") %>% 
   # pivot_longer
   gather(
      key = market_index, 
      value = value, 
      -date
   ) %>% 
   # filter out any weekends 
   mutate(day = wday(date, label = TRUE)) %>% 
   filter(day != "Sat", date <= as.Date("2010-01-01")) %>%
   # convert to tsibble
   as_tsibble(
      key = market_index, 
      index = date
   )
    
vol_tsibble

# train / test split --------------------------------------------------------------------

# filter for 2004 - March 1, 2009 for modelling

vol_tsibble_2004 <- vol_tsibble %>% 
   filter_index("2004-01-02" ~ "2009-09-01")

# plot data -----------------------------------------------------------------------------

filter(vol_tsibble_2004, !str_detect(market_index, "rv")) %>% 
   ggplot(aes(x = date, y = value)) +
   geom_line() +
   facet_wrap(~market_index) +
   labs(title = "Stock Market Indices: Daily Returns 2000 - 2010",
        x = NULL,
        y = "Daily Return") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45),
         strip.background = element_rect(fill = "grey90"))

filter(vol_tsibble, str_detect(market_index, "rv")) %>% 
ggplot(aes(x = date, y = value)) +
   geom_line() +
   facet_wrap(~market_index) +
   labs(title = "Stock Market Indices: Realized Volatility 2000 - 2010",
        x = NULL,
        y = "Realized Volatility") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45),
         strip.background = element_rect(fill = "grey90"))

# ACF plots of Realized Volatility   

# names of ACFs to plot
acfs <- unique(vol_tsibble_2004$market_index) %>% 
   str_subset(".r$")

acf_tsibble <- vol_tsibble_2004 %>% 
   spread(market_index, value) %>% 
   select(date, ends_with(".r")) %>% 
   # square returns for ACF plots
   mutate_at(.vars = vars(ends_with(".r")),
             ~ .^2)

acf_plots <- map(.x = acfs, ~ggAcf(acf_tsibble[.], lag.max = 50) + 
                              labs(title = NULL, 
                                   subtitle = paste0(str_remove(., ".r"), " squared returns")))

# squared returns show strong evidence of volatility clustering
plot_grid(plotlist = acf_plots, ncol = 3)







# Fit AR 1 and simple mean. Simple Mean has higher AIC
fit1 <- Arima(train.8, order = c(1,0,0), include.mean = FALSE)
fit2 <- Arima(train.8, order = c(0,0,0), include.mean = FALSE)
fit1
fit2

# Test for ARCH effects: 1) Ljung-Box with Nul that m lags of 
#                           ACF of squared resids are zero (AutocorTest) 
#                        2) Lagrange Multiplier Test (engle 1982) (ArchTest)
#                           See Tsay book 134 and Tsay (2005) 101-102
AutocorTest(fit1$residuals^2, lag = 12, type = "Ljung-Box")  
ArchTest(fit2$residuals^2, lags = 12, demean = FALSE)


# Model selection: Which Model has Best In-Sample Fit?
# Justify STD vs. Norm in distribution
arch.order <- c(1:5)
arch.names <- paste("arch", arch.order, sep ="")
tarch.order <- c(1:5)
tarch.names <- paste("tarch", arch.order, sep ="")
model.count <- 1
fit.list <- list()
# Fit Arch(1:5)
for(p in arch.order){
  arch.spec <- ugarchspec(variance.model = list(garchOrder = c(p,0)),
                          mean.model = list(armaOrder = c(0,0), 
                                            include.mean = FALSE), 
                          distribution.model = "std")
  
  arch.fit <- ugarchfit(spec = arch.spec, data = train.8, solver = "hybrid")
  
  fit.list[[model.count]] <- arch.fit
  model.count <- model.count + 1
}
#Fit TARCH(1:5) TGARCH Zakoian (1994) found especially effective in Hansen Lunde 2006
for(t in tarch.order){
  tarch.spec <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(t,0), 
                                                 submodel = "TGARCH"),
                           mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                           distribution.model = "std")
  tarch.fit <- ugarchfit(spec = tarch.spec, data = train.8, solver = "hybrid")
  fit.list[[model.count]] <- tarch.fit
  model.count <- model.count + 1
}
names(fit.list) <- c(arch.names, tarch.names)
#fit GARCH11
garch.spec<- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                        distribution.model = "std")
fit.list$garch11 <- ugarchfit(spec = garch.spec, data = train.8, solver = "hybrid")

# fit Thr- GARCH11 TGARCH Zakoian (1994) found especially effective in Hansen Lunde 2006
tgarch.spec<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), 
                                               submodel = "TGARCH"),
                         mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                         distribution.model = "std")
fit.list$tgarch11<- ugarchfit(spec=tgarch.spec, data = train.8, solver = "hybrid")

# collect model data into matrix
info.mat <- sapply(fit.list, infocriteria)
rownames(info.mat)<- rownames(infocriteria(fit.list[[1]]))
info.mat


# demonstrate normailty is poor assumtion. Favor Student-t dist.

# ARCH model by Engle (1982)
# GARCH11 Bollerslev (1986)
# TGARCH Zakoian (1994) found especially effective in Hansen Lunde 2006
arch.order <- c(1:5)
tarch.order <- c(1:5)
roll <- c(5,10,15,20)
model.count <- 1
roll.names <- paste("roll", roll, sep =".")
roll.list <- list()
tic()
for(r in roll){
  for(p in arch.order){
    arch.spec <- ugarchspec(variance.model = list(garchOrder = c(p,0)),
                            mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                            distribution.model = "std")
    arch.roll <- ugarchroll(spec = arch.spec, data = returns.8, n.ahead = 1, forecast.length = 261,
                            n.start = NULL, refit.every = r, refit.window = c("moving"),
                            window.size = NULL, solver = "hybrid", fit.control = list(),
                            solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
                            cluster = NULL, keep.coef = TRUE)
    roll.list[[model.count]] <- arch.roll
    model.count <- model.count+1
  }
  # TARCH 1-5 Zakoian (1994) found especially effective in Hansen Lunde 2006
  for(t in tarch.order){
    tarch.spec <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(t,0), 
                                                   submodel = "TGARCH"),
                             mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                             distribution.model = "std")
    tarch.roll <- ugarchroll(spec = tarch.spec, data = returns.8, n.ahead = 1, forecast.length = 261,
                             n.start = NULL, refit.every = r, refit.window = c("moving"),
                             window.size = NULL, solver = "hybrid", fit.control = list(),
                             solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
                             cluster = NULL, keep.coef = TRUE)
    
    roll.list[[model.count]] <- tarch.roll
    model.count <- model.count + 1
  }
  # Garch (1,1) of Bollerslev (1986)
  garch.spec<- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                          distribution.model = "std")
  roll.list[[model.count]] <- ugarchroll(spec=garch.spec, data=returns.8, n.ahead = 1, forecast.length = 261,
                                         n.start = NULL, refit.every = r, refit.window = c("moving"),
                                         window.size = NULL, solver = "hybrid", fit.control = list(),
                                         solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
                                         cluster = NULL, keep.coef = TRUE)
  model.count<- model.count + 1
  # TGARCH Zakoian (1994) found especially effective in Hansen Lunde 2006
  tgarch.spec<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), 
                                                 submodel = "TGARCH"),
                           mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                           distribution.model = "std")
  roll.list[[model.count]] <- ugarchroll(spec = tgarch.spec, data = returns.8, n.ahead = 1, forecast.length = 261,
                                         n.start = NULL, refit.every = r, refit.window = c("moving"),
                                         window.size = NULL, solver = "hybrid", fit.control = list(),
                                         solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
                                         cluster = NULL, keep.coef = TRUE)
  model.count<- model.count + 1
}
toc()

# Get Volatility Forecasts (Sigma is Conditional Standard Deviation! Square to get conditional variance)
var.mat <- sapply(roll.list, function(x) x@forecast$density[,"Sigma"]^2)
rv.crisis <- coredata(crisis.rv)
sr.crisis <- coredata(s.crisis.r)

# Calculate average MSE using both RV ans Squared Returns as volatility proxies for each model and rolling forecast
mse.rv.mat <- apply(var.mat, MARGIN = 2, function(x) ((rv.crisis-x)^2))
mse.sr.mat <- apply(var.mat, MARGIN = 2, function(x) ((sr.crisis-x)^2))
mean.mse.rv.mat <- apply(mse.rv.mat, MARGIN = 2, mean)
mean.mse.sr.mat <- apply(mse.sr.mat, MARGIN = 2, mean)
mean.mse.rv.mat.1 <- matrix(mean.mse.rv.mat, nrow = 12, ncol = length(roll), byrow = FALSE)#ncol = number of rolls, nrow = # models
mean.mse.sr.mat.1 <- matrix(mean.mse.sr.mat, nrow = 12, ncol = length(roll), byrow = FALSE)
mse.final <- cbind(mean.mse.rv.mat.1, mean.mse.sr.mat.1)
colnames(mse.final)<- c("one.week.rv", "two.week.rv", "three.week.rv", "four.week.rv",
                        "one.week.sr", "two.week.sr", "three.week.sr", "four.week.sr")
rownames(mse.final)<- c("ARCH1", "ARCH2", "ARCH3", "ARCH4", "ARCH5", 
                        "TARCH1", "TARCH2", "TARCH3", "TARCH4", "TARCH5", 
                        "GARCH11", "TGARCH11")

# Calculate average QL using both RV ans Squared Returns as volatility proxies for each model and rolling forecast

ql.rv.mat <- apply(var.mat, MARGIN = 2,
                   function(x) (rv.crisis/x-log(rv.crisis/x)-1) ) # QLIKE FROM BROWNLESS pg.8#(log(x)+(rv.crisis/x))
ql.sr.mat <- apply(var.mat, MARGIN = 2,
                   function(x) (sr.crisis/x-log(sr.crisis/x)-1)) # QLIKE from BROWNLESS pg.8 #(log(x)+(sr.crisis/x))
mean.ql.rv.mat <- apply(ql.rv.mat, MARGIN = 2, mean)
mean.ql.sr.mat <- apply(ql.sr.mat, MARGIN = 2, mean)
mean.ql.rv.mat.1 <- matrix(mean.ql.rv.mat, nrow = 12, ncol = length(roll), byrow = FALSE)
mean.ql.sr.mat.1 <- matrix(mean.ql.sr.mat, nrow = 12, ncol = length(roll), byrow = FALSE)
ql.final <- cbind(mean.ql.rv.mat.1, mean.ql.sr.mat.1)
colnames(ql.final)<- c("one.week.rv", "two.week.rv", "three.week.rv", "four.week.rv",
                       "one.week.sr", "two.week.sr", "three.week.sr", "four.week.sr")
rownames(ql.final)<- c("ARCH1", "ARCH2", "ARCH3", "ARCH4", "ARCH5", 
                       "TARCH1", "TARCH2", "TARCH3", "TARCH4", "TARCH5", 
                       "GARCH11", "TGARCH11")
mse.final
ql.final

# Implement Diebold Mariano Test using GARCH(1,1) as baseline model 
# The null hypothesis is that the two methods have the same forecast accuracy.
# For alternative="less", the alternative hypothesis is that method 2 is less accurate than method 1. 
# For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1. 
# For alternative="two.sided", the alternative hypothesis is that method 1 and method 2 have different levels of accuracy.
alt.errors <- ql.rv.mat[,c(-11,-23,-35,-47)]
p.vec <- c()
test.count <- 1
alternative <- c("greater") #, "two.sided", "greater"
for(m in alternative){
  for(i in 1:ncol(alt.errors)){
    if(i<=11){
      p.vec[test.count] <- dm.test(e1 = ql.rv.mat[,11], e2 = alt.errors[,i], alternative = m, 
                                   h = 1, power = 1)$p.value 
      test.count <- test.count + 1
    }
    if(i>11 & i<=22){
      p.vec[test.count] <- dm.test(e1 = ql.rv.mat[,23], e2 = alt.errors[,i], alternative = m, 
                                   h = 1, power = 1)$p.value 
      test.count <- test.count + 1
    }
    if(i> 22 & i<=33){
      p.vec[test.count] <- dm.test(e1 = ql.rv.mat[,35], e2 = alt.errors[,i], alternative = m, 
                                   h = 1, power = 1)$p.value 
      test.count <- test.count + 1
    }
    if(i>33 & i<=44){
      p.vec[test.count] <- dm.test(e1 = ql.rv.mat[,47], e2 = alt.errors[,i], alternative = m, 
                                   h = 1, power = 1)$p.value 
      test.count <- test.count + 1
    }
  }
}
# Collect all p.values into matrix
p.mat <- matrix(p.vec, nrow = 11, byrow = FALSE)
rownames(p.mat) <- c("ARCH1", "ARCH2", "ARCH3", "ARCH4", "ARCH5", 
                     "TARCH1", "TARCH2", "TARCH3", "TARCH4", "TARCH5", "TGARCH11")
colnames(p.mat) <- c(alternative, alternative, alternative, alternative)
p.mat <- round(p.mat, digits = 4)
p.mat

# DM Test accross different Rolls (1 week, 2 Week, 3 Week, 4 Week) 1 week as basis
dm.vec <- c()
alternative <- c("less") #, "two.sided", "greater"
dm.count <- 1
for(m in alternative){
  for(i in 1:12){
    dm.vec[dm.count] <- dm.test(e1 = ql.rv.mat[,i], e2 = ql.rv.mat[,i+12], alternative = m, 
                                h = 1, power = 1)$p.value 
    dm.count <- dm.count + 1
    dm.vec[dm.count] <- dm.test(e1 = ql.rv.mat[,i], e2 = ql.rv.mat[,i+24], alternative = m, 
                                h = 1, power = 1)$p.value
    dm.count <- dm.count + 1
    dm.vec[dm.count] <- dm.test(e1 = ql.rv.mat[,i], e2 = ql.rv.mat[,i+36], alternative = m, 
                                h = 1, power = 1)$p.value
    dm.count <- dm.count + 1
  }
}
dm.mat<- matrix(dm.vec, nrow = 12, ncol = 3, byrow = TRUE)
colnames(dm.mat) <- c("two.week", "three.week", "four.week")
rownames(dm.mat) <- c("ARCH1", "ARCH2", "ARCH3", "ARCH4", "ARCH5", 
                      "TARCH1", "TARCH2", "TARCH3", "TARCH4", "TARCH5","GARCH11", "TGARCH11")
dm.mat <- round(dm.mat, digits = 4)
dm.mat


#Plotting 7 day rolling forecasts
forecast.1 <- as.xts(var.mat[,1:12], order.by = index(crisis.rv))
names(forecast.1)<- c("ARCH1", "ARCH2", "ARCH3", "ARCH4", "ARCH5", 
                      "TARCH1", "TARCH2", "TARCH3", "TARCH4", "TARCH5", 
                      "GARCH11", "TGARCH11")
plot(forecast.1$TGARCH11)
lines(crisis.rv, col = "red")






vol.forecast<-as.xts((roll.list[[1]]@forecast$density[,"Sigma"])^2, 
                     order.by = index(crisis.rv))
vol.plot<- cbind(crisis.rv,vol.forecast)
names(vol.plot)<- c("RV", "Forecast")
plot.zoo(vol.plot, col =1:2,screens = 1)
legend("topleft", legend = names(vol.plot),          
       col=1:2,
       lty=1,              
       cex=0.85) 

# MSE tends to choose GARCH11
# QL tends to chose ARCH1 

# Brownless 2.3 Forecast evaluation
# Our measure of predictive accuracy is based on the average forecast loss 
# achieved by a model/strategy/proxy triplet. A model that provides a smaller 
# average loss is more accurate and therefore preferred. 

# Tsay pg 143 for Forecasting and Equations example 3.1

# GARCH Modeling
# Engle (1982) and Bollerslev (1986) introduced GARCH models—these account for the volatility clustering that is often seen in the return series of market-priced assets. An example is the popular GARCH(1,1) model:
# where ht is the variance at time t conditional on past information, and εt is the residual at time t. The three parameters of the model are α, β and ω—there would also generally be a parameter for the mean of the series.
# Given an estimate of the parameters for a model, it is desirable to determine if the model adequately explains the variance process. A common approach is to divide each residual by the estimated standard deviation for that time point, and square these standardized residuals. Finally perform a Ljung-Box test on the squared standardized residuals (minus their mean). If the statistic is large, then there is evidence that the model is inadequate. Wong and Li (1995) studied the rank Ljung-Box test in this setting.
# Consider the example of the S&P 500 for dates from 2 January 1985 through 31 December 2001. The return series for this data has 4292 observations. We’ll start with a six-parameter model. The Ljung-Box test statistic with 15 lags for the model is 30.57, giving a p-value of 1%. This is as we expect since the model is known not be very good—it is a GARCH(0,4) model (that is, an ARCH(4) model) assuming a Gaussian distribution for the residuals. This model has four lags of the squared residual and no lags of the conditional variance.
