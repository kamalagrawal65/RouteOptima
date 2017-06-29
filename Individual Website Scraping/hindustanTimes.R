library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)
library(jsonlite)
library(rvest)

# For first time run this
#rD <- rsDriver(port = 4567L, browser = c("chrome", "firefox", "phantomjs", "internet explorer"), version = "latest", chromever = "latest", geckover = "latest", iedrver = NULL, phantomver = "2.1.1", verbose = TRUE, check = TRUE)

selServ <- wdman::selenium(verbose = FALSE)
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)

# Headless - without opening browser
remDr <- remoteDriver(browserName = "phantomjs", port=4567)

# browser wiill open
remDr <- remoteDriver(browserName="chrome", port=4567)

remDr$open()
remDr$navigate("http://www.hindustantimes.com/")

remDr$closeServer

webElem <- remDr$findElement(using = 'name', value = "text")

webElem <- remDr$findElement(using = 'name', value = "search")
webElem$highlightElement()
webElem$sendKeysToElement(list("bargarh", "\uE007"))


# url= remDr$getCurrentUrl()[[1]]
# remDr$navigate(url)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

TitleElems <- remDr$findElements(using = 'css selector', "div.media-heading.headingfour")
titleData <- unlist(lapply(TitleElems, function(x){x$getElementText()}))


ContentElems <- remDr$findElements(using = 'css selector', "div.para-txt")
contentData <- unlist(lapply(ContentElems, function(x){x$getElementText()}))

DateElems <- remDr$findElements(using = 'css selector', "span.time-dt")
dateData <- unlist(lapply(DateElems, function(x){x$getElementText()}))


data11 <- list(titleData = titleData, contentData = contentData, dateData = dateData)

# Create the data frame
min_len <- min(length(data11$dateData),length(data11$contentData),length(data11$titleData))
data_final1 <- data.frame(titleData=head(data11$titleData,min_len), contentData=head(data11$contentData,min_len), dateData=head(data11$dateData,min_len))



# webpage <- read_html(url)
# 
# # # #Fetch Title
# Title_html <- html_nodes(webpage,'.media-heading')
# iterData <- html_text(Title_html)
# titleData <- append(titleData, iterData)