library('httr')
library('RSelenium')
library(wdman)
library(RCurl)
library(XML)
library(urltools)
library(stringr)
library(parsedate)
library(lubridate)
library(rvest)
library(SnowballC)
library(mongolite)
library(properties)

# To be done
# Optimize scrolling websites data fetching


# Server
selServ <- wdman::selenium(verbose = FALSE)
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)
remDr <- remoteDriver(browserName="chrome", port=4567)
remDr$open()

properties_file <- read.properties("E:/gpsdesk/scrape/conf.properties")
separator_in_properties_file=", "

# Keyword Extraction
keywords <- strsplit(properties_file$news_keywords,separator_in_properties_file)[[1]]

# Gender Extraction
genders <- strsplit(properties_file$genders,separator_in_properties_file)[[1]]

# Age group Extraction
age_group <- strsplit(properties_file$age_group,separator_in_properties_file)[[1]]



###################################################################
####################### Functions #################################

# Function which performs scraping of current page
scrap <- function(scrap_url){
  html1 <- getURL(scrap_url, .encoding = "CE_UTF8")
  doc1 <- htmlParse(html1)
  attrs <- xpathApply(doc1, "//div/p", xmlValue)
  attrs <- sapply(attrs, function(x) x[[1]])
  v1 <- as.vector(attrs)
  
  attrs <- xpathApply(doc1, "//div", xmlValue)
  attrs <- sapply(attrs, function(x) x[[1]])
  v2 <- as.vector(attrs)
  
  v2 <-append(v1,v2)
  v2 <- unique(v2)
  
  v2 <- as.matrix(v2)
  v2 <- as.character(v2)
  
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
    vec <- append(vec,io[[i]])
  }
  
  vec=unique(vec)
  
  v2 <- gsub("\r?\n|\r|\t", " ", v2)
  v2 <- trimws(v2, which = c("both", "left", "right"))
  v2 <- unique(v2)
  
  # parse_date(v2)
  tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
  last_date  <-  parse_date(tm)
  
  date_array <- parse_date(v2)
  date_array_length <- length(date_array)
  
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
  contentData  <-  gsub("[[:punct:]]", " ", date_with_data2$v2)
  dateData  <-  date_with_data2$date_array
  
  data_with_data4 <- data.frame(dateData, contentData, stringsAsFactors = F)
  data_with_data4$keyword = NA
  data_with_data4$ageGroup = NA
  data_with_data4$gender = NA
  
  # Replace every word with its stem word
  for(i in 1:dataFrameLength){
    tmp <- strsplit(data_with_data4[,2]," ")
    
    # Stemming
    stemmedString <- (wordStem(tmp[[i]], language="porter"))
    intersectedStrings <- intersect(stemmedString, keywords)
    if(length(intersectedStrings)>0){
      data_with_data4[3][i,] <- (intersectedStrings[length(intersectedStrings)])
    }else{
      data_with_data4[3][i,] <- NA
    }
    
    # Age Group
    intersectedStrings <- intersect(stemmedString, age_group)
    if(length(intersectedStrings)>0){
      data_with_data4[4][i,] <- (intersectedStrings[length(intersectedStrings)])
    }else{
      data_with_data4[4][i,] <- NA
    }
    
    # Gender
    intersectedStrings <- intersect(stemmedString, genders)
    if(length(intersectedStrings)>0){
      data_with_data4[5][i,] <- (intersectedStrings[length(intersectedStrings)])
    }
    else if(!is.na(data_with_data4[4][i,]) & (data_with_data4[4][i,]=="man" || data_with_data4[4][i,]=="men" || data_with_data4[4][i,]=="boy")){
      data_with_data4[5][i,] <- "Male"
    }else if(!is.na(data_with_data4[4][i,]) & (data_with_data4[4][i,]=="woman" || data_with_data4[4][i,]=="women" || data_with_data4[4][i,]=="girl")){
      data_with_data4[5][i,] <- "Female"
    }
    else{
      data_with_data4[5][i,] <- NA
    }
    
    
    #data1[,2][[i]]=paste(wordStem(tmp[[i]]),collapse=" ")
  }
  
  
  # Remove Useless data
  date_with_data3 <- data_with_data4[!is.na(data_with_data4$keyword),]
  # na.omit(data_with_data4)
  # print(date_with_data3)
  output_dataf <<- rbind(output_dataf, date_with_data3)
  print(output_dataf)
}


####################################################################

# Function for crawling
crawler <- function(recur_url, depth){
  if(depth>THRESHOLD_DEPTH)
    return(NA)
  
  scrap(recur_url)

  remDr$navigate(recur_url)
  # Fetch all links in current Page
  # All links stored in urlLinks
  finalurls <- pagination(recur_url)
  
  # Crawl each page
  final_urlLinks_length <- length(finalurls$attrs)
  
  if(final_urlLinks_length>0){
    for(i in 1:final_urlLinks_length){
      if(is.na(domain(finalurls$attrs[i])) && (grepl("^/",finalurls$attrs[i]))){
        finalurls$attrs[i] <- paste(base_url,finalurls$attrs[i],sep="")
      }
      
      if(!grepl("^http",finalurls$attrs[i])){
        finalurls$attrs[i] <- paste("http://", finalurls$attrs[i],sep="")
      }
      
      if(is.null(visited_links[[finalurls$attrs[i]]])){
        visited_links[[finalurls$attrs[i]]] <<- T
        crawler(finalurls$attrs[i] ,depth+1)
      }
    }
    
  }
  
}

##################################################################

# Finds all pagination links
pagination <- function(pagination_url){
  main.page <- read_html(x = pagination_url)
  urls <- main.page %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  textss <- main.page %>% 
    html_nodes("a") %>% 
    html_text()
  
  attrs <- trimws(urls, which = c("both", "left", "right"))
  attrs1 <- trimws(textss, which = c("both", "left", "right"))
  
  paginationdf <- data.frame(attrs,attrs1,stringsAsFactors = F)
  paginationdf <- paginationdf[grep("^\\d{1,2}$", paginationdf$attrs1),]
  paginationdf <- unique(paginationdf)
  paginationdf <- paginationdf[!paginationdf$attrs=="#",]
  # Add base url if not present
  return(paginationdf)
}


####################################################################

# Find latitude and longitude of a location
getLatLon <- function(location){
  BgDr <- remoteDriver(browserName = 'phantomjs', port=4567)
  BgDr$open()
  
  url_loc <- "http://www.latlong.net/"
  BgDr$navigate(url_loc)
  
  webElem <- BgDr$findElement(using="id", value="gadres")
  webElem$sendKeysToElement(list(as.character(location)))
  
  webElem <- BgDr$findElement(using="class",value = 'button')
  webElem$clickElement()
  
  
  webElem <- BgDr$findElement(using="name",value = 'lat')
  webElem$clickElement()
  webElem$sendKeysToElement(list(key = "control", "a"))
  webElem$sendKeysToElement(list(key = "control", "c"))
  latitude=(readClipboard())
  
  webElem <- BgDr$findElement(using="name",value = 'lng')
  webElem$clickElement()
  webElem$sendKeysToElement(list(key = "control", "a"))
  webElem$sendKeysToElement(list(key = "control", "c"))
  longitude=(readClipboard())
  
  return (list(lat=latitude, lng=longitude))
}

#####################################################################
#####################################################################



############### Main Program ##################


# Urls
urls <- strsplit(properties_file$urls,separator_in_properties_file)[[1]]

# Default Url
base_url <- character()
searchKeyword <- "crime"
THRESHOLD_DEPTH <- as.numeric(properties_file$depth)


# Ouput Data
output_dataf <- data.frame(dateData=as.Date(character()),
                           contentData=character(), 
                           keyword=character(), 
                           ageGroup=character(),
                           gender=character(),
                           referenceWebsite=character(),
                           stringsAsFactors=FALSE) 


# A map to keep track of visited links
visited_links <- list()


length_of_urls=length(urls)

for(i in 1:length_of_urls){

  url <- urls[i]
  base_url <<- domain(url)
  
  # Start working
  remDr$navigate(url)
  page1_html <- getURL(url, .encoding = "CE_UTF8")
  page1_doc <- htmlParse(page1_html)
  
  # Search for textbox
  attrs <- xpathApply(page1_doc, "//input", xmlAttrs)
  length=length(attrs)
  
  # Run this if statement. Program begins here
  # Go for input box i.e search box
  if(length!=0){
    for (j in 1:length){
      if((is.na(attrs[[j]]["type"]) || attrs[[j]]["type"]=="text") && (!is.na(attrs[[j]]["name"]))){
        remDr$navigate(url)
        webElem <- remDr$findElement(using = 'name', value = attrs[[j]]["name"])
        webElem$sendKeysToElement(list(searchKeyword, "\uE007"))
        Sys.sleep(2)
        
        # Get current url to access its elements
        result_url <- remDr$getCurrentUrl()[[1]]
        
        # Crawl it
        crawler(result_url, 1)
        
      }
    }
  }

  
}



# Process final Data
final_output_data <- output_dataf
final_output_data$contentData <- trimws(final_output_data$contentData, which = c("both", "left", "right"))
final_output_data = unique(final_output_data)


# Append Latitude and longitude in the data
# Call getLatLon function
latlon=getLatLon("Koramangala")

final_output_data$latitude <- latlon$lat
final_output_data$longitude <- latlon$lng
final_output_data$referenceWebsite <- url


# Extract date
f <- function(s) strsplit(as.character(s), " " )[[1]][1]
final_output_data$dateOnly <- sapply(final_output_data$dateData, f)

# Extract time
f <- function(s) strsplit(as.character(s), " " )[[1]][2]
final_output_data$timeOnly <- sapply(final_output_data$dateData, f)




# MONGO DB DATABASE INSERTION
mongo_url=properties_file$mongo_url
mongo_database=properties_file$mongo_database
mongo_collection=properties_file$mongo_collection

# Insert data into mongod
db <- mongo(mongo_collection, 
            mongo_database,
            mongo_url)

db$find()
db$insert(final_output_data)
