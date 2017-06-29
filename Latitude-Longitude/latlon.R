library('httr')
library('RSelenium')

# Set up server
selServ <- wdman::selenium(verbose = FALSE)
elServ <- wdman::selenium(retcommand = TRUE, verbose = FALSE)
#remDr <- remoteDriver(browserName="chrome", port=4567)
remDr <- remoteDriver(browserName = 'phantomjs', port=4567)
remDr$open()


url="http://www.latlong.net/Show-Latitude-Longitude.html"

remDr$navigate(url)

lat=12.9434
lon=77.6214

for(i in 1:3000){
  remDr$navigate(url)
  
  lat=lat+0.0001
  lon=lon+0.0001
  
  webElem <- remDr$findElement(using="name", value="lat")
  webElem$sendKeysToElement(list(as.character(lat)))
  
  webElem <- remDr$findElement(using="name", value="lng")
  webElem$sendKeysToElement(list(as.character(lon)))
  
  webElem <- remDr$findElement(using="class",value = 'margin38')
  webElem$clickElement()
  
  
  webElem <- remDr$findElement(using="id",value = 'address')
  #webElem$highlightElement()
  
  webElem$clickElement()
  webElem$sendKeysToElement(list(key = "control", "a"))
  
  webElem$sendKeysToElement(list(key = "control", "c"))
  print(i)
  print(readClipboard())
}