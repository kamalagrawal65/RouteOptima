# # Install the following packages for the first time

library(lubridate)
library(rvest)
library(SnowballC)

interval_in_months <- 6


###############################################
##############################################
########## Times of India ######################

timesofIndia <- function(){
  # url to fetch data
  url <- 'http://timesofindia.indiatimes.com/topic/begur-road'
  
  # Initialize vectors
  dateData <- c()
  titleData <- c()
  contentData <- c()
  
  
  # Last Date whose data needs to be collected
  today <- Sys.Date()
  subtract_value <- paste("-",as.character(interval_in_months),sep="")
  last_date <- seq(as.Date(today), length = 2, by = paste(subtract_value,"months"))[2] 
  
  i=1
  
  
  while(i<10){
    search_url=paste(url,as.character(i),sep="/")
    
    response<- GET(url)
    if(response$status_code==404 || response$status_code==403){
      break
    }
    
    #Reading the HTML code from the website
    webpage <- read_html(search_url)
    
    # # #Fetch Title
    Title_html <- html_nodes(webpage,'.content .title')
    iterData <- html_text(Title_html)
    titleData <- append(titleData, iterData)
    
    # Fetch Date
    date_html <- html_nodes(webpage,'.content .meta')
    iterData <- html_text(date_html)
    dateData <- append(dateData, iterData)
    dates <- as.Date(dateData, "%b %d")
    dates[dates > as.Date(today)]=dates[dates > as.Date(today)]-years(1)
    
    # # #Fetch Content
    content_html <- html_nodes(webpage,'.content p')
    iterData <- html_text(content_html)
    contentData <- append(contentData, iterData)
    
    
    # Check if date condition holds
    if(as.Date(dates[length(dates)]) < as.Date(last_date))
      break
    
    i=i+1
  }
  
  return(list(titleData = titleData, contentData = contentData, dateData = dates))
}
#data <- timesofIndia()

############ END of Times of India ##################
####################################################
###################################################





################################################
################################################
########### Deccan Chronicle ####################

deccanChronicle <- function(){
  # url to fetch data
  url <- 'http://www.deccanchronicle.com/search?srh=news&search=koramangala&pg='
  
  # Initialize vectors
  dateData <- c()
  titleData <- c()
  contentData <- c()
  
  
  # Last Date whose data needs to be collected
  today <- Sys.Date()
  subtract_value <- paste("-",as.character(interval_in_months),sep="")
  last_date <- seq(as.Date(today), length = 2, by = paste(subtract_value,"months"))[2] 
  
  i=1
  
  
  while(i<10){
    search_url=paste(url,as.character(i),sep="")
    
    #Reading the HTML code from the website
    webpage <- read_html(search_url)
    
    # # #Fetch Title
    Title_html <- html_nodes(webpage,'.SunChroListH3')
    iterData <- html_text(Title_html)
    titleData <- append(titleData, iterData)
    
    # Fetch Date
    date_html <- html_nodes(webpage,'.SunChDt2')
    iterData <- html_text(date_html)
    dateData <- append(dateData, iterData)
    # dateUnit=strsplit(dateData," ")[[1]]
    dates <- as.Date(dateData, "%d %b %Y %I:%M %p")
    
    # # #Fetch Content
    content_html <- html_nodes(webpage,'.OpinionStrapList')
    iterData <- html_text(content_html)
    contentData <- append(contentData, iterData)
    
    # Check if date condition holds
    if(as.Date(dates[length(dates)]) < as.Date(last_date))
      break
    
    i=i+1
  }
  
  return(list(titleData = titleData, contentData = contentData, dateData = dates))
}


# data <- deccanChronicle()

##########################################################
##########################################################
##########################################################






########################## Indian Express ###################
#############################################################
#############################################################

indianExpress <- function(){
  # Initialize vectors
  dateData <- c()
  titleData <- c()
  contentData <- c()
  
  # url to fetch data
  url <- 'http://indianexpress.com/page'
  
  # Last Date whose data needs to be collected
  interval_in_months <- 6
  today <- Sys.Date()
  subtract_value <- paste("-",as.character(interval_in_months),sep="")
  last_date <- seq(as.Date(today), length = 2, by = paste(subtract_value,"months"))[2] 
  
  i=1
  
  
  while(i<7){
    search_url=paste(url,as.character(i),"?s=indiranagar",sep="/")
    
    tryCatch(read_html(search_url), error=function(e) break)
    
    #Reading the HTML code from the website
    webpage <- read_html(search_url)
    
    
    # # #Fetch Title
    Title_html <- html_nodes(webpage,'h3')
    iterData <- html_text(Title_html)
    titleData <- append(titleData, iterData)
    
    # # Fetch Date
    date_html <- html_nodes(webpage,'time')
    iterData <- html_text(date_html)
    dateData <- append(dateData, iterData)
    
    # 
    # # dateUnit=strsplit(dateData," ")[[1]]
    # dates <- as.Date(dateData, "%d %b %Y %I:%M %p")
    
    # # #Fetch Content
    content_html <- html_nodes(webpage,'div .details p')
    iterData <- html_text(content_html)
    contentData <- append(contentData, iterData)
    
    # # Check if date condition holds
    # if(as.Date(dates[length(dates)]) < as.Date(last_date))
    #   break
    
    i=i+1
  }
  
  return(list(titleData = titleData, contentData = contentData, dateData = dates))
}

##########################################################
##########################################################
##########################################################




###############################################################################
########################## Start of Analysis ##################################
###############################################################################

#Call functions here to get the data

# Optional
# source("E:/gpsdesk/scrape/deccanChronicle.R")
# source("E:/gpsdesk/scrape/indianExpress.R")
# source("E:/gpsdesk/scrape/timesOfIndia.R")


# append data to dataframes -- to be done properly
data <- deccanChronicle()
data <- timesofIndia()
data <- append(data,timesofIndia())
data <- append(data,indianExpress())


# Keywords to look For
keywords <- c("crime", "accid", "sexual", "murder", "rape", "rob", "kidnap", "beat", "suicide", "kill", "stab", "rain", "rainfall")
priority_keywords <- list("crime" = 1,
                          "accid" = 2,
                          "sexual" = 5,
                          "murder" = 4,
                          "rape" = 5,
                          "rob" = 3,
                          "kidnap" = 3,
                          "beat" = 4,
                          "suicide" = 2,
                          "kill" = 5,
                          "stab" = 6,
                          "rain" = 3,
                          "rainfall" = 3)


search_pattern <- paste(keywords, collapse = "|")


# Create the data frame
min_len <- min(length(data$dateData),length(data$contentData),length(data$titleData))
data_final <- data.frame(titleData=tail(data$titleData,min_len), contentData=tail(data$contentData,min_len), dateData=tail(data$dateData,min_len))



# Replace the escape characters found
data_final <- data.frame(lapply(data_final, gsub, pattern = "\n|\t", replacement = ""), stringsAsFactors = FALSE)
data1 <- data_final[grepl(search_pattern, data_final$contentData), ]

data1 <- as.data.frame(cbind(titleData = gsub("[[:punct:]]", " ", data1$titleData), contentData = gsub("[[:punct:]]", " ", data1$contentData), dateData = data1$dateData), stringsAsFactors = F)

dataFrameLength <- length(data1$contentData)

data1$keyword = NA



# Replace every word with its stem word
for(i in 1:dataFrameLength){
  tmp <- strsplit(data1[,2]," ")
  stemmedString <- (wordStem(tmp[[i]], language="porter"))
  intersectedStrings <- intersect(stemmedString, keywords)
  
  
  if(length(intersectedStrings)>0){
    data1[4][i,]=intersectedStrings[length(intersectedStrings)]
  }else{
    data1[4][i,]=NA
  }
  #data1[,2][[i]]=paste(wordStem(tmp[[i]]),collapse=" ")

}


# Remove Useless data
data1 <- na.omit(data1)


# Set Day of Incidents
data1$dayData <- weekdays(as.Date(data1$dateData))


# Insert data into mongod
db <- mongo(collection = 'crimeAnalysis', 
            db = 'RouteOptimaAnalysis',
            url = 'mongodb://localhost:27017')
db$insert(data1)


# Update the Priority Value
lengthOfData = length(data1$keyword)
pvalue=0
for(i in 1:lengthOfData){
  pvalue=pvalue+(priority_keywords[[data1$keyword[i]]])
}



# Searching for a data in Column
column_value="Wednesday"
subset(data1, dayData == column_value)
subset(data1, keyword == "accid")











################### TF- IDF ############################

# TF-IDF
# counters <- table(unlist(strsplit(tolower(data1$contentData), " ")))
# 
# for (i in 1:length(keywords)) {
#   print(counters[keywords[i]])
# }


# Function to substitute a word
#as.data.frame(sapply(data, function(x) gsub("years", "fewasdfdasd", x)))









############# Extra ################################

# pg <- read_html("http://timesofindia.indiatimes.com/topic/Koramangala-Crime")
# html_data <- html_nodes(pg, "body")
# unique(str_match_all(html_text(html_nodes(pg, "body")),"(be[[:alnum:]_]+)")[[1]][,2])
# (str_match_all(html_text(html_nodes(pg, "body")),"(div)")[[1]][,2])




