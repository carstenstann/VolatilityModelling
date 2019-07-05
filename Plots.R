library(readr)
library(dplyr)
library(broom)
library(tibble)
library(xts)
library(lubridate)
library(FinTS)
library(forecast)
library(rugarch)
library(tictoc)

## Import data and convert dates
import <- as_tibble(read_csv("/Users/carsten 1/Desktop/Forecasting Paper/OxfordManRealizedVolatilityIndices.csv", 
                             skip = 2))
import$DateID <- as.character(import$DateID)
import$DateID<- as_date(import$DateID, format = "%Y%m%d")

## Subset RV and Return data, convert to XTS objects
RV.na <- import %>% select(SPX2.rv, FTSE2.rv, N2252.rv, GDAXI2.rv, DJI2.rv, FCHI2.rv) %>% 
  as.xts(order.by = import$DateID) ##FTSE2.rv, N2252.rv, GDAXI2.rv,DJI2.rv, FCHI2.rv

returns.na <- import %>% select(SPX2.r, FTSE2.r, N2252.r, GDAXI2.r,DJI2.r, FCHI2.r) %>% 
  as.xts(order.by = import$DateID) ## FTSE2.r, N2252.r, GDAXI2.r,DJI2.r, FCHI2.r

## Interpolate NAs linearly
RV <- na.approx(RV.na)
returns <- na.approx(returns.na)
## Clip first and last observation due to NAs
RV<- RV[-1,]
RV <- RV[-length(RV),]
returns <- returns[-1,]
returns <- returns[-length(returns),]

## fix column names
colnames(RV) <- c("SP500", "FTSE100", "Nikkei225", "DAX", "DJIA", "CAC40") ##"FTSE100", "Nikkei225", "DAX", "DJI", "CAC40"
colnames(returns) <- c("S&P 500", "FTSE 100", "Nikkei225", "DAX", "DJIA", "CAC 40") ##"FTSE100", "Nikkei225", "DAX", "DJI", "CAC40"


## subset data for training and test sets (4 and 8 yrs Sept.1 2000/2004 - March1, 2009)
## Test set is 2008-09-01 through 2009-09-01
start.8 <- as.Date("2000-09-01")
start.4 <- as.Date("2004-09-01") 
start.test <- as.Date("2008-09-01")
end <- as.Date("2009-09-01")
train.8 <-subset(returns, index(returns)>= start.8 & index(returns) < start.test)
train.4 <-subset(returns, index(returns)>= start.4 & index(returns) < start.test)
returns.8 <- subset(returns, index(returns)>= start.8 & index(returns) < end)
returns.4 <- subset(returns, index(returns)>= start.4 & index(returns) < end)

## Subset Volatility Proxies
RV.8 <- subset(RV, index(RV)>= start.8 & index(RV) < end)
RV.4 <- subset(RV, index(RV)>= start.4 & index(RV) < end)
crisis.rv <-subset(RV, index(RV) >= as.Date("2008-09-01") & index(RV) < end)
crisis.r <-subset(returns, index(returns)>= start.test & index(returns) < end) ## subset of returns for test set
s.crisis.r <- crisis.r^2 ## squared returns for test set

## Plot data
plot.zoo(returns.8, main = "Daily Returns")
plot.zoo(RV.8)
plot.zoo(crisis.r, main = "Daily Returns")
plot.zoo(crisis.rv)

## Plot ACFS
## ACF of returns
par(mfrow = c(2,3))
Acf.returns.1 <- Acf(returns.8[,1], main = "S&P 500") %>% tidy()
Acf.returns.2 <- Acf(returns.8[,2], main = "FTSE 100") %>% tidy()
Acf.returns.3 <- Acf(returns.8[,3], main = "Nikkei 225") %>% tidy()
Acf.returns.4 <- Acf(returns.8[,4], main = "DAX") %>% tidy()
Acf.returns.5 <- Acf(returns.8[,5], main = "DJIA") %>% tidy()
Acf.returns.6 <- Acf(returns.8[,6], main = "CAC 40") %>% tidy()
## ACFs of squared returns
Acf.returns2.1 <- Acf(returns.8[,1]^2, main = "S&P 500") %>% tidy()
Acf.returns2.2 <- Acf(returns.8[,2]^2, main = "FTSE 100") %>% tidy()
Acf.returns2.3 <- Acf(returns.8[,3]^2, main = "Nikkei 225") %>% tidy()
Acf.returns2.4 <- Acf(returns.8[,4]^2, main = "DAX") %>% tidy()
Acf.returns2.5 <- Acf(returns.8[,5]^2, main = "DJIA") %>% tidy()
Acf.returns2.6 <- Acf(returns.8[,6]^2, main = "CAC 40") %>% tidy()

## ACF returns during crisis
Acf.returns.SP500.c <- Acf(crisis.r[,1]^2, lag.max = 50, main = "S&P 500") %>% tidy()
Acf.returns2.2 <- Acf(crisis.r[,2]^2, main = "FTSE 100") %>% tidy()
Acf.returns2.3 <- Acf(crisis.r[,3]^2, main = "Nikkei 225") %>% tidy()
Acf.returns2.4 <- Acf(crisis.r[,4]^2, main = "DAX") %>% tidy()
Acf.returns2.5 <- Acf(crisis.r[,5]^2, main = "DJIA") %>% tidy()
Acf.returns2.6 <- Acf(crisis.r[,6]^2, main = "CAC 40") %>% tidy()

## reset plots
par(mfrow=c(1,1))

## Fit AR 1 and simple mean. Simple Mean has higher AIC
aic.vec <- c()
fit.list <- list()
count <- 1
for(i in 1:6){
    fit1 <- Arima(train.8[,i], order = c(1,0,0), include.mean = FALSE)
    fit2 <- Arima(train.8[,i], order = c(0,0,0), include.mean = FALSE)
    fit.list[[count]] <- fit2
    count<- count + 1
    aic1<- fit1$aic
    aic2<- fit2$aic
    aic.vec <- c(aic.vec, aic1, aic2)
}
aic.mat <- matrix(aic.vec, ncol = 6, nrow = 2, byrow = FALSE)
colnames(aic.mat)<- c("SP500", "FTSE100", "Nikkei225", "DAX", "DJIA", "CAC40")
rownames(aic.mat)<- c("AR1", "Zero Mean Model")

## Test for ARCH effects: 1) Ljung-Box with Nul that m lags of 
##                           ACF of squared resids are zero (AutocorTest) 
##                        2) Lagrange Multiplier Test (engle 1982) (ArchTest)
##                           See Tsay book 134 and Tsay (2005) 101-102
p.vec <- c()
for(i in 1:6){
    print(auto.cor <- AutocorTest(fit.list[[i]]$residuals^2, lag=12, type = "Ljung-Box"))  
    print(arch.test <- ArchTest(fit.list[[i]]$residuals^2, lags=12, demean = FALSE))
    ##p.vec <- c(p.vec, auto.cor$p.value, arch.test$p.value)
}
p.mat <- matrix(p.vec, ncol = 6, nrow = 2, byrow = FALSE)
colnames(p.mat)<- c("SP500", "FTSE100", "Nikkei225", "DAX", "DJIA", "CAC40")
rownames(p.mat)<- c("AutocorTest", "ARCH Test")