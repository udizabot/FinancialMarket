
###########################################################################
# Portfolio Selection -----------------------------------------------------

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
# Create the Portfolio Object ---------------------------------------------
Portfolio <- portfolio.spec(assets = tickers)


# Adding Optimization Constraint ------------------------------------------
Portfolio <- add.constraint(portfolio = Portfolio, type = "full_investment") # Pesos iguais
Portfolio <- add.constraint(portfolio = Portfolio, type = "long_only") # Não permite venda à descoberto


# Adding Optimization Objectives ------------------------------------------
Portfolio <- add.objective(portfolio = Portfolio, type = "return", name = "mean") # Retorno esperado através da média amostral
Portfolio <- add.objective(portfolio = Portfolio, type = "risk", name = "StdDev") # Risco esperado através do desvio padrão



###########################################################################
# Portfolio Optimization --------------------------------------------------

Optimal <- optimize.portfolio(Returns, 
                              portfolio = Portfolio, 
                              optimize_method = "random", 
                              trace = T)


chart.RiskReward(Optimal, 
                 risk.col = "StdDev", return.col = "mean", 
                 chart.assets = T, 
                 main = "Risco x Retorno")


## Pesos ótimos da carteira
Optimal$weights

## Valores ótimos associados 
Optimal$opt_values


