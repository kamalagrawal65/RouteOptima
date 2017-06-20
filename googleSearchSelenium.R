# install RSelenium

library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)
# For first time run this
#rD <- rsDriver(port = 4567L, browser = c("chrome", "firefox", "phantomjs", "internet explorer"), version = "latest", chromever = "latest", geckover = "latest", iedrver = NULL, phantomver = "2.1.1", verbose = TRUE, check = TRUE)

selServ <- wdman::selenium(verbose = FALSE)
#selServ$log()
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)


remDr <- remoteDriver(browserName="chrome", port=4567)
remDr$open()
# remDr$open(silent=T)
remDr$navigate("http://www.google.com/")

# remDr$goBack()
# remDr$getCurrentUrl()
# remDr$goForward()
webElem <- remDr$findElement(using = 'class', value = "gsfi")
webElem$highlightElement()
# For search
#webElem$sendKeysToElement(list("R Cran"))

webElem$clearElement()

# with enter
webElem$sendKeysToElement(list("begur road crime", "\uE007"))

webElem$sendKeysToElement(list("R Cran", key = "enter"))


webElems <- remDr$findElements(using = 'css selector', "h3.r")

webElems <- remDr$findElements(using = 'css selector', "div.rc")

datw <- remDr$findElements(using = 'css selector', "span.f")


webElem$sendKeysToElement(list(key = "end"))


resHeaders <- unlist(lapply(datw, function(x){x$getElementText()}))

resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))

remDr$getCurrentUrl()
rm(remDr)


Title_html <- html_nodes(page,'span .f')





url <- remDr$getCurrentUrl()[[1]]
page <- GET(url)

webpage <- read_html(url)
print(http_status(page))
page_text <- content(page, as='text')
grepl('Wells', page_text, ignore.case=T)



html <- getURL(url)
doc <- htmlParse(html)

attrs <- xpathApply(doc, "//h3//a[@href]", xmlAttrs)
links <- sapply(attrs, function(x) x[[1]])