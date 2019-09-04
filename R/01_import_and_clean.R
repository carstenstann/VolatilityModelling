#########################################################################################
## Project: VolatilityModelling
## Script purpose: import equity index data
## Date: 03.09.2019
## Author: Carsten Stann
#########################################################################################

library(quantmod)
library(tidyverse)
library(timetk)
library(xts)

# Import data ---------------------------------------------------------------------------

getSymbols(Symbols = c("^GSPC", "^DJI", "^FTSE", "^GDAXI","^FCHI", "^N225"), 
           src = "yahoo", 
           from = "2000-01-01",
           auto.assign = TRUE)

# clean data ----------------------------------------------------------------------------

indices <- Ad(cbind(GSPC, DJI, FTSE, GDAXI, FCHI, N225)) %>% 
   na.approx(na.rm = FALSE) %>% 
   na.omit()

returns <- diff(log(indices))[-1,]["2000/"] %>% 
   tk_tbl(rename_index = "date") 

nested_returns <- returns %>% 
   gather(market_index, return, -date) %>% 
   group_by(market_index) %>% 
   nest()

rm(list = c("GSPC", "DJI", "FTSE", "GDAXI", "FCHI", "N225"))