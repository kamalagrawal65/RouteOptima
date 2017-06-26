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

webElem <- remDr$findElement(using="name", value="lat")
webElem$sendKeysToElement(list(as.character(12.9434)))

webElem <- remDr$findElement(using="name", value="lng")
webElem$sendKeysToElement(list(as.character(77.6214)))

webElem <- remDr$findElement(using="class",value = 'margin38')
webElem$clickElement()


webElem <- remDr$findElement(using="id",value = 'address')
#webElem$highlightElement()

webElem$clickElement()
webElem$sendKeysToElement(list(key = "control", "a"))

webElem$sendKeysToElement(list(key = "control", "c"))

readClipboard()
