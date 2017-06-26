library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)
library(urltools)
library(stringr)

# Server
selServ <- wdman::selenium(verbose = FALSE)
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)
remDr <- remoteDriver(browserName="chrome", port=4567)
remDr$open()

# Keywords
url = "http://indianexpress.com/"
searchKeyword = "Koramangala Crime"
base_url= domain(url)
THRESHOLD_DEPTH=3

# Keyword Extraction
keywords <- c("horror", "shot", "crime", "accid", "sexual","fire", "murder", "rape", "rob", "kidnap", "beat", "suicide", "kill", "stab", "rain", "rainfall")

# Start working
remDr$navigate(url)
page1_html <- getURL(url, .encoding = "CE_UTF8")
page1_doc <- htmlParse(page1_html)

# Search for textbox
attrs <- xpathApply(page1_doc, "//input", xmlAttrs)
length=length(attrs)


# Ouput Data
output_dataf <- data.frame(dateData=as.Date(character()),
                            contentData=character(), 
                            keyword=character(), 
                            stringsAsFactors=FALSE) 

# Go for input box i.e search box
if(length!=0){
  for (i in 1:length){
    if(is.na(attrs[[i]]["type"]) || attrs[[i]]["type"]=="text"){
      remDr$navigate(url)
      webElem <- remDr$findElement(using = 'name', value = attrs[[i]]["name"])
      webElem$sendKeysToElement(list(searchKeyword, "\uE007"))
      Sys.sleep(2)
      # Get current url to access its elements
      result_url= remDr$getCurrentUrl()[[1]]
      
      # Crawl it
      crawler(result_url, 1)
    }
  }
}


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
  #print(date_with_data3)
  output_dataf <<- rbind(output_dataf, date_with_data3)
  print("fban\n")
  print(output_dataf)
}



crawler <- function(recur_url, depth){
  if(depth>THRESHOLD_DEPTH)
    return(NA)
  
  scrap(recur_url)
  
  remDr$navigate(recur_url)
  webElem$sendKeysToElement(list(key = "end"))
  webElem$sendKeysToElement(list(key = "end"))
  recur_html <- getURL(recur_url, .encoding = "CE_UTF8")
  recur_doc <- htmlParse(recur_html)
  
  # Fetch all links in current Page
  # All links stored in urlLinks
  urlLinks <- xpathSApply(recur_doc, '//a', xmlGetAttr, "href")
  
  #those who don't have base url attached to it attach it
  
  urlLinks <- trimws(urlLinks, which = c("both", "left", "right"))
  urlLinks <- unique(urlLinks)
  urlLinksDf <- as.data.frame(urlLinks, stringsAsFactors = F)
  finalurls <- urlLinksDf$urlLinks[domain(urlLinksDf$urlLinks)==base_url]
  
  # final links to process
  finalurls <- finalurls[!is.na(finalurls)]
  
  # Crawl each page
  final_urlLinks_length <- length(finalurls)
  
  for(i in 1:final_urlLinks_length){
    crawler(finalurls[i] ,depth+1)
  }
  
}