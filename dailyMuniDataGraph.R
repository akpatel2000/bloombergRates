library(XML)
library(lubridate)
library(RMySQL)
library(dbConnect)
library(httr)
library(ggplot2)
library(grid)
library(gridExtra)


## Dataframe prep
currentDate <- as.Date(now())


## Write to database and read from database the rates file
db1 <- dbConnect(MySQL(), user= "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
historicalRates <- dbReadTable(db1, name = "rates")
numberRows <- nrow(historicalRates)
dbDisconnect(db1)

historicalRates$date <- as.Date(historicalRates$date)
rateRecord <- historicalRates[numberRows,]


## Calculate spline curve from current yield curve
muniCurve <- spline(c(1,2,5,10,30), 
                    c(rateRecord$muniYield1Y, rateRecord$muniYield2Y, rateRecord$muniYield5Y, rateRecord$muniYield10Y, rateRecord$muniYield30Y), 
                    n=30, method = "natural")

muniCurve <- as.data.frame(muniCurve)
names(muniCurve) <- c("Maturity", "AAA_Yield")
muniCurve$AAA_Yield <- round(muniCurve$AAA_Yield, digits = 2)


## Calculate yield x days aga
xDays = 182
dateXDaysAgo <- historicalRates$date[numberRows] - days(x=xDays)
ratesXDaysAgo <- historicalRates[historicalRates$date == dateXDaysAgo,]

## Some days there are more than one record -- always take the last
ratesXDaysAgo <- ratesXDaysAgo[dim(ratesXDaysAgo)[1],]
muniCurveXDaysAgo <- spline(c(1,2,5,10,30), 
                            c(ratesXDaysAgo$muniYield1Y, ratesXDaysAgo$muniYield2Y, ratesXDaysAgo$muniYield5Y, ratesXDaysAgo$muniYield10Y, ratesXDaysAgo$muniYield30Y), 
                            n=30, method = "natural")
muniCurveXDaysAgo <- as.data.frame(muniCurveXDaysAgo)
names(muniCurveXDaysAgo) <- c("Maturity", "old_AAA_Yield")
df <- data.frame(muniCurve, muniCurveXDaysAgo[,2])


# ## Graph the data
g1 <- ggplot(data=historicalRates, aes(x=date, y=muniYield10Y)) + 
  geom_line(col="blue") + 
  ylab("Yield of 10 year Generic AAA Muni Bond") + 
  theme(axis.title.y = element_text(size = 11, face = "bold", color = "red")) +
  geom_hline(yintercept = rateRecord$muniYield10Y, color = "green")


g2 <- ggplot(data=df, aes(x=Maturity)) + geom_point(aes(y=AAA_Yield), col="green") + 
  geom_point(aes(y= muniCurveXDaysAgo[,2]), col = "blue") + 
  geom_vline(xintercept = c(5,10,15,20,25,30), col = "orange", alpha = .5) + 
  ylab("AAA Muni Yield") + xlab("Maturity") + 
  theme(axis.title.x = element_text(color="red", face="bold", size=10)) + 
  theme(axis.title.y = element_text(color="red",face="bold", size = 10)) + 
  labs(caption = paste0("(Green = ", currentDate,  " : Blue = ",xDays, " Days Ago)"))


g3 <- ggplot(data=historicalRates, aes(x=date, y=treasuryYield10Y)) + 
  geom_line(col="blue") + 
  ylab("Yield of 10 year Treasury Bond") + 
  theme(axis.title.y = element_text(size = 11, face = "bold", color = "red")) + 
  geom_hline(yintercept = rateRecord$treasuryYield10Y, color = "green")


tbl <- cbind(muniCurve[1:15,], muniCurve[16:30,])

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = .75)),
  colhead = list(fg_params=list(cex = .75)),
  rowhead = list(fg_params=list(cex = .75)))
tbl1 <- tableGrob(tbl, theme = mytheme, rows = NULL)

grid.arrange(g1,g3,g2,tbl1, nrow=2, ncol=2, as.table= TRUE)

fileName <- paste0("Muni",currentDate,".pdf")