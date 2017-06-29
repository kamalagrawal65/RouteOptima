library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)

selServ <- wdman::selenium(verbose = FALSE)
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)

remDr <- remoteDriver(browserName="chrome", port=4567)
remDr$open()

url = "http://indianexpress.com/"
url_pattern=c("https://", "http://")

searchKeyword = "Koramangala Crime"

remDr$navigate(url)
url= remDr$getCurrentUrl()[[1]]

html <- getURL(url, .encoding = "CE_UTF8")
doc <- htmlParse(html)

# Search for textbox
attrs <- xpathApply(doc, "//input", xmlAttrs)
length=length(attrs)


if(length!=0){
  for (i in 1:length){
    if(is.na(attrs[[i]]["type"]) || attrs[[i]]["type"]=="text"){
      webElem <- remDr$findElement(using = 'name', value = attrs[[i]]["name"])
      webElem$sendKeysToElement(list(searchKeyword, "\uE007"))
      
      # Fetch all links in current Page
      # All links stored in urlLinks
      urlLinks <- xpathSApply(doc, '//a', xmlGetAttr, "href")
      urlLinksDf <- as.data.frame(urlLinks, stringsAsFactors = F)
      urlLinks <- urlLinksDf[grepl(paste(url_pattern,collapse = "|"), urlLinksDf$urlLinks), ]
      
      
      
    }
  }
}