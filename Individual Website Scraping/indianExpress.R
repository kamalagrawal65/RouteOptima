# Handle Page Exceed error


# Install rvest
# Install lubridate
library(lubridate)
library(rvest)

# url to fetch data
url <- 'http://indianexpress.com/page'

indianExpress <- function(){
  # Initialize vectors
  dateData <- c()
  titleData <- c()
  contentData <- c()
  
  
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


#data <- indianExpress()
########################################################
#########################################################

