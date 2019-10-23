## Daily capture of Bloomberg Rates both Munis and Treasuries
## Dependents -- Proprietary MySQL database

library(XML)
library(lubridate)
library(RMySQL)
library(dbConnect)
library(httr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(dplyr)

Sys.setenv(TZ='EST')

## Scrape Bloomberg Screen
bloombergMarketRatesURL <- "http://www.bloomberg.com/markets/rates-bonds/government-bonds/us"
bloombergMarketRatesHTML <- htmlTreeParse(rawToChar(GET(bloombergMarketRatesURL)$content), useInternalNodes = TRUE)

muniYields <- as.numeric(sub("%", "", (xpathSApply(bloombergMarketRatesHTML, 
                        "//div[@data-view-uid='1|0_5_9']//td[@data-type='percent']", 
                        xmlValue))))

treasuryYields <- as.numeric(sub("%", "", (xpathSApply(bloombergMarketRatesHTML, 
                            "//div[@data-view-uid='1|0_5_3']//td[@data-type='percent']", 
                            xmlValue))))

muniYield1Y <- muniYields[1]
muniYield2Y <- muniYields[2]
muniYield5Y <- muniYields[3]
muniYield10Y <- muniYields[4]
muniYield30Y <- muniYields[5]

treasuryYield3M <- treasuryYields[1]
treasuryYield6M <- treasuryYields[2]
treasuryYield1Y <- treasuryYields[3]
treasuryYield2Y <- treasuryYields[4]
treasuryYield5Y <- treasuryYields[5]
treasuryYield10Y <- treasuryYields[6]
treasuryYield30Y <- treasuryYields[7]


##Check if muni and treasury record are valid by testing 10 year point
if (is.na(muniYield10Y)) {
  stop("Error muni record not good")
}

if (is.na(treasuryYield10Y)) {
  stop("Error treasury record is very bad Brian")
}


## Build record
date <- Sys.Date()

rateRecord <- data.frame(date, 
                              muniYield1Y, 
                              muniYield2Y, 
                              muniYield5Y, 
                              muniYield10Y, 
                              muniYield30Y,
                                  treasuryYield3M, 
                                  treasuryYield6M, 
                                  treasuryYield1Y, 
                                  treasuryYield2Y, 
                                  treasuryYield5Y, 
                                  treasuryYield10Y, 
                                  treasuryYield30Y)


## Write to database and read from database the rates file
if (nrow(rateRecord) > 0 ) {
    db1 <- dbConnect(MySQL(), user= "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
    dbWriteTable(db1, rateRecord, name = "rates", append = TRUE, row.names = FALSE)
    historicalRates <- dbReadTable(db1, name = "rates")
    numberRows <- nrow(historicalRates)
    dbDisconnect(db1)
    message("rateRecord write succesful")
} else {
    stop("rateRecord not written -- rateRecord length is zero")
}


## Calculate spline curve from current yield curve
muniCurve <- spline(c(1,2,5,10,30), 
               c(muniYield1Y, muniYield2Y, muniYield5Y, muniYield10Y, muniYield30Y), 
               n=30, method = "natural")

muniCurve <- as.data.frame(muniCurve)
names(muniCurve) <- c("Maturity", "AAA_Yield")

muniCurve$AAA_Yield <- round(muniCurve$AAA_Yield, digits = 2)

prevCurve <- historicalRates[nrow(historicalRates)-1,]
muniYield1Y <- prevCurve$muniYield1Y
muniYield2Y <- prevCurve$muniYield2Y
muniYield5Y <- prevCurve$muniYield5Y
muniYield10Y <- prevCurve$muniYield10Y
muniYield30Y <- prevCurve$muniYield30Y
prevCurve <- spline(c(1,2,5,10,30), 
                    c(muniYield1Y, muniYield2Y, muniYield5Y, muniYield10Y, muniYield30Y), 
                    n=30, method = "natural")
prevCurve <- as.data.frame(prevCurve)
prevCurve$y <- round(prevCurve$y, digits = 2) 
muniCurve <- cbind(muniCurve, prevCurve$y)
names(muniCurve) <- c("Maturity","AAA_Yield",  "Prev_AAA_Yield")
muniCurve <- muniCurve %>% mutate(yield_chg = muniCurve$AAA_Yield - muniCurve$Prev_AAA_Yield)
muniCurve$yield_chg <- round(muniCurve$yield_chg, digit = 2)

tbl1 <- tableGrob(muniCurve, theme = ttheme_default(8), rows = NULL)
# grid.table(muniCurve, theme = ttheme_default(9), rows = NULL)


## Calculate yield x days aga
xDays = 182
dateXDaysAgo <- as.Date(historicalRates$date[numberRows]) - days(x=xDays)
ratesXDaysAgo <- historicalRates[as.Date(historicalRates$date) == dateXDaysAgo,]
## Some days there are more than one record -- always take the last
ratesXDaysAgo <- ratesXDaysAgo[dim(ratesXDaysAgo)[1],]
muniCurveXDaysAgo <- spline(c(1,2,5,10,30), 
                            c(ratesXDaysAgo$muniYield1Y, ratesXDaysAgo$muniYield2Y, ratesXDaysAgo$muniYield5Y, ratesXDaysAgo$muniYield10Y, ratesXDaysAgo$muniYield30Y), 
                            n=30, method = "natural")
muniCurveXDaysAgo <- as.data.frame(muniCurveXDaysAgo)
names(muniCurveXDaysAgo) <- c("Maturity", "AAA_Yield")

## Graph the data
plt1 <- ggplot(data= historicalRates, aes(x= as.Date(date), y=muniYield10Y)) + 
    geom_line(col="blue") +
    xlab("Date") + ylab("10Y Muni AAA Yield")
plt2 <- ggplot(data= historicalRates, aes(x= as.Date(date), y=treasuryYield10Y)) + 
    geom_line(col="blue") +
    xlab("Date") + ylab("10Y Treasury Yield")

ggarrange(tbl1, ggarrange(plt1, plt2, ncol = 1, nrow =2), ncol = 2, nrow = 1)
rm(tbl1)
rm(plt1)
rm(plt2)
rm(historicalRates)