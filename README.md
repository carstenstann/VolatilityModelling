An Evaluation of Value-at-Risk Forecasting Methods
================

## Introduction

Volatility permeates the economy. It influences the pricing of assets
and plays a prominent role in financial decision-making. One area of
application is Value-at-Risk (VaR) modelling, which seeks to quantify
the potential financial losses and the probability of their occurance.
Firms and regulators alike use VaR as a risk management tool. However,
simple VaR models fail to capture volatility clustering, thereby
overestimating risk in calm times and underestimating risk in volatile
times.

One solution to this problem is to feed VaR models conditional
(time-varying) variance forecasts. This analysis uses eight different
Generalized Autoregressive Conditional Heteroskedasticity (GARCH) model
specifications to conduct daily VaR forecasts for 6 major stock market
indices.

## Data

This analysis considers daily prices of 6 major stock market indices:

  - S\&P 500
  - Dow Jones Industrial Average
  - FTSE 100
  - DAX Performance-Index
  - CAC 40
  - Nikkei 225

All data is retrieved from Yahoo using the `quantmod` package for the
period January 1, 2019 to September 15, 2019. The level as well as daily
log-returns of each index is plotted below.

**Stock Market Indices Daily Price: 2000 - 2019**
<img src="plots/README/README-price_by_index-1.png" width="100%" />

**Stock Market Index Returns: 2000 - 2019**
<img src="plots/README/README-log_return_by_index-1.png" width="100%" />

Each log-return series exhibits heteroscedasticity or volatiltiy
clustering, the phenomenon first described by Mandelbrot (1963): “large
changes tend to be followed by large changes of either sign, and small
changes tend to be followed by small changes.” Periods of high
volatility are clearly visible in late 2008 as well as in 2002.

The ACF and PACF plots of each index’s squared returns show serial
correlations, indicating daily returns are not independent and
signalling the presence of
ARCH-effects.

<img src="plots/README/README-acf_returns_by_index-1.png" width="100%" />

<img src="plots/README/README-pacf_squared_returns-1.png" width="100%" />

## Models

The analysis considers eight univariate GARCH model specifications. Each
model assumes returns are normally distributed with a time-varying
variance.

![MeanModel](./plots/README/MeanModel.png)

![models](./plots/README/VolModels.png)

## Forecasts

All models and forecasts are computed using the `rugarch` package.

<img src="plots/README/README-forecast_plot-1.png" width="100%" />
