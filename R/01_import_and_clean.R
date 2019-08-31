##################################################
## Project: Volatility Modelling
## Script purpose: import and clean raw data
## Date: 31.08.2019
## Author: Carsten Stann
##################################################

library(tidyverse) 
library(timetk)
library(lubridate) 
library(xts)

# Import data ---------------------------------------------------------------------------

raw_import <- read_csv("data/OxfordManRealizedVolatilityIndices.csv", skip = 2)

# clean data ----------------------------------------------------------------------------

vol_data <- raw_import %>%
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
   ) %>% 
   # interpolate NAs and convert to tsibble for modelling
   tk_xts(silent = TRUE) %>% 
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
   select(-day)

# create nested tibble
nested_vol_data <- vol_data %>% 
   group_by(market_index) %>% 
   nest()

# remove raw data from environment
rm("raw_import")
