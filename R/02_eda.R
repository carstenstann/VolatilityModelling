##########################################################
## Project: Volatility Modelling
## Script purpose: explore processed data before modelling
## Date: 04.09.2019
## Author: Carsten Stann
##########################################################

library(forecast)
library(dynlm)
library(cowplot)
library(GGally)
library(lmtest)
library(FinTS)
library(broom)

# run import and clean script ----------------------------------------------------------
source("./R/01_import_and_clean.R") # included packages: tidyverse, timetk, lubridate, xts

# inspect cleaned data 
indices
returns
nested_returns

# plot data -----------------------------------------------------------------------------

# market indices
indices %>% 
   tk_tbl(rename_index = "date") %>% 
   gather(market_index, price, -date) %>% 
   mutate(market_index = fct_relabel(market_index, ~str_remove(., ".Adjusted"))) %>% 
ggplot(aes(x = date, y = price)) +
   geom_line() +
   facet_wrap(~market_index, scale = "free_y") +
   labs(title = "Stock Market Indices Daily Price: 2000 - 2019",
        x = NULL,
        y = "Price (local currency)") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45),
         strip.background = element_rect(fill = "grey90"))

ggsave("./plots/price_by_index.png", width = 12, height = 7, dpi = 320)

# returns
nested_returns %>% 
   unnest(cols = data) %>% 
   ungroup() %>% 
   mutate(market_index = fct_relabel(market_index, ~str_remove(., ".Adjusted"))) %>% 
ggplot(aes(x = date, y = return)) +
   geom_line() +
   facet_wrap(~market_index) +
   labs(title = "Stock Market Index Returns: 2000 - 2019",
        x = NULL,
        y = "Daily Log Return") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45),
         strip.background = element_rect(fill = "grey90"))

ggsave("./plots/log_return_by_index.png", width = 12, height = 7, dpi = 320)

# correlation plots ---------------------------------------------------------------------

ggpairs(returns[,-1])

ggsave("./plots/corrplot_returns.png", width = 12, height = 7, dpi = 320)

# ACF plots of squared reurns -----------------------------------------------------------  

# names of ACFs to plot
squared_returns<- returns %>% 
   # calculate square returns
   mutate_if(is.numeric,~ .^2)

acf_squared_returns <- map(names(squared_returns)[-1], ~ggAcf(squared_returns[[.]], lag.max = 50) + 
                                          labs(title = NULL,
                                               y = NULL,
                                               subtitle = paste0(str_remove(., ".Adjusted"), " squared returns")))

# sub grid with ACF plots
acf_grid <- plot_grid(plotlist = acf_squared_returns, ncol = 3)

# create title 
title_acf <- ggdraw() + 
   draw_label("ACF of Stock Market Index Squared returns",
              fontface = 'bold',
              x = 0,
              hjust = 0
   ) +
   theme(plot.margin = margin(0, 0, 0, 7))

# plot ACF of squared returns by index
plot_grid(title_acf, acf_grid, ncol = 1, rel_heights = c(0.05, 1))

ggsave("./plots/acf_returns_by_index.png", width = 12, height = 7, dpi = 320)

# PACF plots of squared reurns -----------------------------------------------------------  

pacf_squared_returns <- map(names(squared_returns)[-1], ~ggPacf(squared_returns[[.]], lag.max = 50) + 
                              labs(title = NULL,
                                   y = NULL,
                                   subtitle = paste0(str_remove(., ".Adjusted"), " squared returns")))

# sub grid with ACF plots
pacf_grid <- plot_grid(plotlist = pacf_squared_returns, ncol = 3)

# create title 
title_pacf <- ggdraw() + 
   draw_label("PACF of Stock Market Index Squared returns",
              fontface = 'bold',
              x = 0,
              hjust = 0
   ) +
   theme(plot.margin = margin(0, 0, 0, 7))

# plot ACF of squared returns by index
plot_grid(title_pacf, pacf_grid, ncol = 1, rel_heights = c(0.05, 1))

ggsave("./plots/pacf_returns_by_index.png", width = 12, height = 7, dpi = 320)


# test for ARCH effects -----------------------------------------------------------------

htest <- nested_returns %>% 
   mutate(data_ts = map(.x = data,
                        .f = tk_ts,
                        select = -date)) %>% 
   mutate(dynlm_mean_model = map(data_ts, ~dynlm(. ~ 1)),
          dynlm_ar_1 = map(data_ts, ~dynlm(. ~ L(., 1))),
          # Ljung Box test for autocorrelation in errors 
          ljung_box_test = map(dynlm_ar_1, ~Box.test(x = .$residuals^2), type = "Ljung-Box"),
          # Breusch Godfrey test for autocorrelation of errors
          bg_test = map(.x = dynlm_ar_1, .f = bgtest),
          # Breusch Pagan test fo heteroscedasticity
          bp_test = map(.x = dynlm_ar_1, .f = bptest, studentize = FALSE),
          # Arch-LM test for heteroscedasticity
          arch_lm_test = map(data_ts, ~ArchTest(., demean = FALSE))
   ) %>% 
   select(-c(data, data_ts, dynlm_mean_model, dynlm_ar_1)) %>% 
   mutate(ljung_box_test = map(ljung_box_test, tidy),
          bg_test = map(bg_test, tidy),
          bp_test = map(bp_test, tidy),
          arch_lm_test = map(arch_lm_test, tidy)) %>% 
   gather(key = test, value = statistic, -market_index) %>% 
   unnest()

# reject null of no autocorrelation, no heteroskedasticity in all series at 0.05 level
# reject null of no arch effect in all series
filter(htest, p.value >= 0.05)

# breusch godfrey test fails to reject null at .01 level for FTSE, DAX, Nikkei returns
filter(htest, p.value >= 0.01)

