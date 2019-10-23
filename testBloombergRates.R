library('httr')

library('RSelenium')
setwd("~/Applications/BloombergRates")
system("docker run -d -p 4445:4444 selenium/standalone-chrome")
Sys.sleep(3)

# Start Selenium standalone server
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "chrome")


remDr$open()


# txt = remDr$getPageSource()

library('XML')
master <- c()
n <- 1
i <- 1
# site <- paste0("https://www.fidelity.com/fund-screener/evaluator.shtml#!&ntf=N&ft=BAL_all&msrV=advanced&sortBy=FUND_MST_MSTAR_CTGY_NM&pgNo=",i)
site <- 'https://www.bloomberg.com/markets/rates-bonds/government-bonds/us'
remDr$navigate(site) # navigates to webpage
# elem <- remDr$findElement(using="id", value="tbody")
elem <- remDr$findElement(using="class", value="data-tables")
elem$highlightElement()
elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
fundList <- unlist(xpathApply(elemxml, '//td[@data-type]', xmlGetAttr, 'next-value'))
master <- c(master, fundList)


remDr$close()
system("docker stop $(docker ps -q)")