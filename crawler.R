library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)
library(parsedate)
library(stringr)
library(lubridate)
library(rvest)
library(SnowballC)

# Set up server
selServ <- wdman::selenium(verbose = FALSE)
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)
remDr <- remoteDriver(browserName="chrome", port=4567)
remDr$open()

# Variables
url_pattern=c("https://", "http://")
searchKeyword = "indiranagar news"
url = "http://ask.com"
THRESHOLD_DEPTH = 4
remove_keywords=c("<",">","//")
cer_file <- "E:/gpsdesk/scrape/cacert.pem"

# Keyword Extraction
keywords <- c("horror", "shot", "crime", "accid", "sexual","fire", "murder", "rape", "rob", "kidnap", "beat", "suicide", "kill", "stab", "rain", "rainfall")


# Crawling function
# For each link, these three things has to be done
# 1. Scrap current page
# 2. Get all links in current page
# 3. Search for textbox in every links
scrap <- function(html1){
  doc1 <- htmlParse(html1)
  attrs <- xpathApply(doc1, "//div/p", xmlValue)
  attrs <- sapply(attrs, function(x) x[[1]])
  v1=as.vector(attrs)
  
  attrs <- xpathApply(doc1, "//div", xmlValue)
  attrs <- sapply(attrs, function(x) x[[1]])
  v2=as.vector(attrs)
  
  # v2=append(v1,v2)
  v2=unique(v2)
  
  if(length(v2)==0){
    return(list())
  }
  
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
  
  
  tfa <- lapply(date_with_data1$v2, function(x) str_extract(x, paste(remove_keywords, collapse = "|")))
  
  date_with_data2=date_with_data1[!grepl(paste(remove_keywords, collapse = "|"), date_with_data1$v2),]
  
  dataFrameLength <- length(date_with_data2$date_array)
  
  if(length(date_with_data2$date_array)==0){
    return(list())
  }
  
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
  print(date_with_data3)
}





crawler <- function(search_url, depth){
  # Check threshold value
  if(depth>THRESHOLD_DEPTH){
    return(NA)
  }
  
  # Check if page Exists
  response<- GET(search_url)
  if(response$status_code==404 || response$status_code==403){
    return(NA)
  }
  
  
  # ######################### Task 2 ##########################
  # # Search for textbox
  remDr$navigate(search_url)
  
  # Get current url to access its elements
  result_url= remDr$getCurrentUrl()[[1]]
  
  # Process current page
  result <- tryCatch({
    html <- getURL(result_url, .encoding = "CE_UTF8", cainfo=cer_file)
    doc <- htmlParse(html)
  }, warning = function(w) {
    
  }, error = function(e) {
    
  }, finally = {
    
  })
  
  
  scrap(html)
  
  textbox_attrs <- xpathApply(doc, "//input", xmlAttrs)
  tb_length=length(textbox_attrs)
  
  # Put the text in every text box
  if(tb_length!=0){
    for (i in 1:tb_length){
      
      if(is.na(textbox_attrs[[i]]["type"]) || textbox_attrs[[i]]["type"]=="text"){
        # Each time we need home page
        remDr$navigate(search_url)
        # Now fetch input box in root page
        
        result <- tryCatch({
          webElem <- remDr$findElement(using = 'name', value = textbox_attrs[[i]]["name"])
          webElem$sendKeysToElement(list(searchKeyword, "\uE007"))
          #Sleep for 1s as wait for page to load
          Sys.sleep(1)
          print(remDr$getCurrentUrl()[[1]])
          # crawl this page also
          crawler(remDr$getCurrentUrl()[[1]], depth+1)
        }, warning = function(w) {
          
        }, error = function(e) {
          
        }, finally = {
          
        })
        
      }
      
    }
    
  }
  
  # ##################### End of Task 2 ######################
  
  
  
  ###################### Task 1 #########################
  # Navigate to required url
  remDr$navigate(search_url)
  
  # Get current url to access its elements
  result_url= remDr$getCurrentUrl()[[1]]
  
  # Process current page
  result <- tryCatch({
    html <- getURL(result_url, .encoding = "CE_UTF8", cainfo=cer_file)
    doc <- htmlParse(html)
  }, warning = function(w) {
    
  }, error = function(e) {
    
  }, finally = {
    
  })
  
  # Fetch all links in current Page
  # All links stored in urlLinks
  urlLinks <- xpathSApply(doc, '//a', xmlGetAttr, "href")
  temp <- as.matrix(urlLinks)
  urlLinksDf <- as.data.frame(temp, stringsAsFactors = F)
  final_urlLinks <- urlLinksDf[grepl("^(http|https)://", urlLinksDf$V1), ]
  final_urlLinks_length <- length(final_urlLinks)
  
  
  # Now call crawler for each link and increase depth
  for(i in 1:final_urlLinks_length){
    crawler(final_urlLinks[i] ,depth+1)
  }
  
  
  ##################### End of Task 1 ####################

  
}

crawler(url,1)

