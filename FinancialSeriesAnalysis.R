
###########################################################################
# Times Series Analysis of Past Rates of Return ---------------------------

## See Bodie, Kane & Marcus (2018)


###########################################################################
# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, quantmod, 
               PerformanceAnalytics, PortfolioAnalytics)

# atualizado

###########################################################################
# Getting Time Series ------------------------------------------------------

tickers <- c("RURA11.SA", "RZAG11.SA", "XPCA11.SA", "KNCA11.SA", "JGPX11.SA", "EGAF11.SA", "DCRA11.SA", "BBGO11.SA", "VGIA11.SA")

Prices <- getSymbols(tickers,
                     auto.assign = TRUE,
                     warnings = FALSE,
                     from = as.Date("2016-12-01"),
                     src = "yahoo") %>%
    map(~Cl(get(.))) %>% reduce(merge) %>% `colnames<-`(tickers)


## Se preferir, converta os preços para períodicidade semanal
WeekPrices <- to.daily(Prices, indexAt = "lastof", OHLC = FALSE)


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
chart.Bar(Returns)

## Gráfico BoxPlot
chart.Boxplot(Returns)

## Gráfico de Correlações
chart.Correlation(Returns)

## Gráfico de Retornos Acumulados
chart.CumReturns(Returns, wealth.index = T, legend.loc = "topleft")

## Histograma 
chart.Histogram(Returns$DCRA11.SA, breaks = 100)

## QQ Plot
chart.QQPlot(Returns$DCRA11.SA)

## Gráfico de Risco-Retorno
chart.RiskReturnScatter(Returns)



charts.PerformanceSummary(Returns)




