
###########################################################################
# Times Series Analysis of Past Rates of Return ---------------------------

## See Bodie, Kane & Marcus (2018)


###########################################################################
# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, quantmod, 
               PerformanceAnalytics, PortfolioAnalytics)



###########################################################################
# Getting Time Series ------------------------------------------------------

tickers <- c("ITSA4.SA", "TAEE11.SA", "ABEV3.SA", "BBAS3.SA", "BRSR6.SA")

Prices <- getSymbols(tickers,
                     auto.assign = TRUE,
                     warnings = FALSE,
                     from = as.Date("2016-12-01"),
                     src = "yahoo") %>%
    map(~Cl(get(.))) %>% reduce(merge) 


## Se preferir, converta os preços para períodicidade semanal
WeekPrices <- to.weekly(Prices, indexAt = "lastof", OHLC = FALSE)


###########################################################################
# Compute Returns ---------------------------------------------------------

Returns <- Return.calculate(WeekPrices, method = "log") %>% na.omit()


###########################################################################
# Expected Returns --------------------------------------------------------

## Com dados históricos, o Retorno Esperado é dado pela média das taxas de retornos

## Cabe uma análise descritiva
table.Stats(Returns)


###########################################################################
# Variance & Standard Deviation -------------------------------------------
StdDev(Returns)


###########################################################################
# Retorno Acumulado -------------------------------------------------------
Return.cumulative(Returns)


# Retorno Anualizado (Para Comparar Retornos) -----------------------------
Return.annualized(Returns)




###########################################################################
# Reward-to-Volatility Ratio (Sharpe Ratio) -------------------------------

## A Razão Sharpe é um indicador do retorno por unidade de risco

SharpeRatio(Returns, p = .99, FUN = "StdDev")



###########################################################################
# Graphical Analysis ------------------------------------------------------

## Gráfico de Retornos no Período
chart.Bar(Returns$ITSA4.SA.Close)

## Gráfico BoxPlot
chart.Boxplot(Returns)

## Gráfico de Correlações
chart.Correlation(Returns)

## Gráfico de Retornos Acumulados
chart.CumReturns(Returns, wealth.index = T, legend.loc = "topleft")

## Histograma 
chart.Histogram(Returns, breaks = 100)

## QQ Plot
chart.QQPlot(Returns)

## Gráfico de Risco-Retorno
chart.RiskReturnScatter(Returns)



charts.PerformanceSummary(Returns)




