
###########################################################################
# Capital Asset Pricing Model ---------------------------------------------

## See Bodie, Kane & Marcus (2018)


###########################################################################
# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, quantmod, tidyquant, timetk, broom,
               PerformanceAnalytics, PortfolioAnalytics)


###########################################################################
# Getting Time Series ------------------------------------------------------

tickers <- c("ITSA4.SA", "TAEE11.SA", "ABEV3.SA", "BBAS3.SA", "BRSR6.SA")

Prices <- getSymbols(tickers,
                     auto.assign = TRUE,
                     warnings = FALSE,
                     from = as.Date("2016-12-01"),
                     src = "yahoo") %>%
    map(~Ad(get(.))) %>% reduce(merge) %>% `colnames<-`(tickers)


## Se preferir, converta os preços para períodicidade semanal
MonthlyPrices <- to.monthly(Prices, indexAt = "lastof", OHLC = FALSE)


###########################################################################
# Compute Returns ---------------------------------------------------------

Returns <- Return.calculate(MonthlyPrices, method = "log") %>% na.omit()



###########################################################################
# Market Portfolio --------------------------------------------------------

Ibovespa <- getSymbols("^BVSP", 
                       auto.assign = TRUE,
                       warnings = FALSE,
                       from = as.Date("2016-12-01"),
                       src = "yahoo")

ReturnIbov <- BVSP$BVSP.Adjusted %>% 
    `colnames<-`('Ibovespa') %>%
    to.monthly(indexAt = 'lastof', OHLC = FALSE) %>%
    Return.calculate(method = "log") %>%
    na.omit()



###########################################################################
# Compute Beta  -----------------------------------------------------------

CAPM.beta(Ra = Returns, Rb = ReturnIbov, Rf = 0)





###########################################################################
# Portfolio Return --------------------------------------------------------

PortfolioReturn <- Return.portfolio(Returns)

PortfolioReturn_tbl <- PortfolioReturn %>%
    tk_tbl(preserve_index = TRUE,
           rename_index = "date")


ReturnIbov_tbl <- ReturnIbov %>%
    tk_tbl(preserve_index = TRUE,
           rename_index = "date") %>%
    rename(ibov_return = Ibovespa)


PortfolioReturn_tbl %>%
    left_join(ReturnIbov_tbl, by = "date") %>%
    ggplot(aes(x = ReturnIbov, y = PortfolioReturn))+
    geom_point(color = "black")+
    geom_smooth(method = "lm",
                se = T, color = "red", size = .5)+
    labs(title = "Retornos do Portfólio x Retornos da Ibovespa",
         x = "", y = "") + theme_minimal()





















    
