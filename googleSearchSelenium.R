# install RSelenium
library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)
library(parsedate)
library(qdap)
library(lubridate)
library(rvest)
library(SnowballC)

# For first time run this
rD <- rsDriver(port = 4567L, browser = c("chrome", "firefox", "phantomjs", "internet explorer"), version = "latest", chromever = "latest", geckover = "latest", iedrver = NULL, phantomver = "2.1.1", verbose = TRUE, check = TRUE)
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



##################################################################
##################################################################



url = "http://www.ask.com/web?q=koramangala+crime&o=0&qo=homepageSearchBox"

url= "https://www.google.com/search?q=google+crawler&oq=google+crwler&aqs=chrome.1.69i57j0l5.6059j0j7&sourceid=chrome&ie=UTF-8#q=koramangala+crime"

url="http://indianexpress.com/?s=Koramangala+Crime"

url="http://www.hindustantimes.com/"

url="http://www.oneindia.com/topic/crime"

doc1 <- htmlTreeParse(url)
trim <- function(x) gsub("^\\s+|\\s+$", "", x) # trim whitespace from start & end
txt <- xpathApply(doc, "//div[last()]", xmlValue)



html <- getURL(url, .encoding = "CE_UTF8")

webpage <- read_html(url)
ddase_html <- html_nodes(webpage,'a')
dd <- html_attr(ddase_html,"data-href")

doc <- htmlParse(html)

# To extract attributes information from webpage
attrs <- xpathApply(doc, "//h3//a[@href]", xmlAttrs)











#####################################################
#####################################################



attrs <- xpathApply(doc, "//div/p", xmlValue)
attrs <- sapply(attrs, function(x) x[[1]])
v1=as.vector(attrs)
v1=gsub("\r?\n|\r", " ", v1)

attrs <- xpathApply(doc, "//div", xmlValue)
attrs <- sapply(attrs, function(x) x[[1]])
v2=as.vector(attrs)
v2=gsub("\r?\n|\r|\t", " ", v2)
v2=trimws(v2, which = c("both", "left", "right"))



#####################################################
#####################################################






attrs <- xpathApply(doc, "//div", xmlValue)
attrs <- sapply(attrs, function(x) x[[1]])
v2=as.vector(attrs)
v2=gsub("\r?\n|\r|\t", " ", v2)
v2=trimws(v2, which = c("both", "left", "right"))
v2=unique(v2)

library(stringr)
tttt <- as.data.frame(v2, stringsAsFactors = F)
t4 <- lapply(tttt, function(x) str_extract(x, "[:alpha:]{1,8}\\s...\\s[:digit:]{4}"))

tttt$v2[!is.na(t4$v2)]

datatmp <- tttt[grepl("[:alpha:]{3}\\s...\\s[:digit:]{4}", tttt), ]


data <- sapply(attrs, function(x) x)














links <- sapply(attrs, function(x) x[[1]])
links <- grep("https://", attrs, fixed = TRUE, value=TRUE)


# Note xmlValue is used to extract value information from webpage
attrs <- xpathSApply(doc, "//div[@class='g']", xmlValue)
#node <- xmlNode(doc, attrs=c(class="f"))

attrs <- xpathApply(doc, "//input", xmlAttrs)


cat(paste(attrs, collapse = "\n"))


url_pattern=c("https://", "http://")

y <- xpathSApply(doc, '//a', xmlGetAttr, "href")
tttt <- as.data.frame(y, stringsAsFactors = F)
t2 <- tttt[grepl(paste(url_pattern,collapse = "|"), tttt$y), ]




tttt <- as.data.frame(attrs, stringsAsFactors = F)
# regex
t2 <- tttt[grepl("\\d{1,2}\\s...\\s....\\s", tttt$attrs), ]

tttt <- as.data.frame(links, stringsAsFactors = F)
t2 <- tttt[grepl("http://", tttt$attrs), ]



# Extract Date
# To get all dates
t4 <- lapply(tttt$attrs, function(x) str_extract(x, "\\d{1,2}\\s[:alpha:]{3}\\s[:digit:]{4}"))





tttt$v2[!is.na(t4$v2)]








io<-strsplit(v2, '\n|\t')
lenn<-length(io)

vec<-c()
for(i in 1:lenn){
  vec=append(vec,io[[i]])
}
vec=unique(vec)
v2=gsub("\r?\n|\r|\t", "", v2)
v2=trimws(v2, which = c("both", "left", "right"))
v2=unique(v2)

tr<-as.data.frame(v2)

tr[sapply(strsplit(as.character(tr$v2)," "),length)<100,]

grepl("\\d\\d * [:alpha:]",tr$v2)









###############################################################
##############################################################

url = "http://www.ask.com/web?q=koramangala+crime&o=0&qo=homepageSearchBox"
url= "https://www.google.com/search?q=google+crawler&oq=google+crwler&aqs=chrome.1.69i57j0l5.6059j0j7&sourceid=chrome&ie=UTF-8#q=koramangala+crime"
url="http://indianexpress.com/?s=Koramangala+Crime"
url="http://www.hindustantimes.com/"
url="http://www.oneindia.com/topic/crime"
url="http://www.telegraph.co.uk/crime/"

url="http://www.newskarnataka.com/bangalore"
url="http://indianexpress.com/about/bangalore-crime/"

html <- getURL(url, .encoding = "CE_UTF8")
doc <- htmlParse(html)

attrs <- xpathApply(doc, "//div/p", xmlValue)
attrs <- sapply(attrs, function(x) x[[1]])
v1=as.vector(attrs)

attrs <- xpathApply(doc, "//div", xmlValue)
attrs <- sapply(attrs, function(x) x[[1]])
v2=as.vector(attrs)

v2=append(v1,v2)
v2=unique(v2)

tttt <- as.data.frame(v2, stringsAsFactors = F)
t4 <- parse_date(v2)
t4 <- as.data.frame(t4)

v2<-tttt$v2[!is.na(t4$t4)]

io<-strsplit(v2, '\n|\t|\r')
lenn<-length(io)

vec<-c()
for(i in 1:lenn){
  vec=append(vec,io[[i]])
}

vec=unique(vec)

v2=gsub("\r?\n|\r|\t", " ", v2)
v2=trimws(v2, which = c("both", "left", "right"))
v2=unique(v2)

# parse_date(v2)
tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
last_date = parse_date(tm)

date_array=parse_date(v2)
date_array_length=length(date_array)

for(i in 1:date_array_length){
  if(is.na(date_array[i]))
    date_array[i]=as.POSIXct(last_date)
  else
    last_date=date_array[i]
}


date_with_data=data.frame(date_array,v2, stringsAsFactors = F)
date_with_data1=date_with_data[sapply(strsplit(as.character(date_with_data$v2)," "),length)<150,]

remove_keywords=c("<",">","//")
tfa <- lapply(date_with_data1$v2, function(x) str_extract(x, paste(remove_keywords, collapse = "|")))

date_with_data2=date_with_data1[!grepl(paste(remove_keywords, collapse = "|"), date_with_data1$v2),]


# Keyword Extraction
keywords <- c("horror", "shot", "crime", "accid", "sexual","fire", "murder", "rape", "rob", "kidnap", "beat", "suicide", "kill", "stab", "rain", "rainfall")

dataFrameLength <- length(date_with_data2$date_array)
date_with_data2$keyword = NA

# Create final df
contentData = gsub("[[:punct:]]", " ", date_with_data2$v2)
dateData = date_with_data2$date_array

data_with_data4 <- data.frame(dateData, contentData, stringsAsFactors = F)
data_with_data4$keyword = NA

# Replace every word with its stem word
for(i in 1:dataFrameLength){
  tmp <- strsplit(data_with_data4[,2]," ")
  
  stemmedString <- (wordStem(tmp[[i]], language="porter"))
  intersectedStrings <- intersect(stemmedString, keywords)
  
  if(length(intersectedStrings)>0){
    data_with_data4[3][i,]=(intersectedStrings[length(intersectedStrings)])
  }else{
    data_with_data4[3][i,]=NA
  }
  #data1[,2][[i]]=paste(wordStem(tmp[[i]]),collapse=" ")
}


# Remove Useless data
date_with_data3 <- na.omit(data_with_data4)