* Pagination.R is the final file. run_program() is the entry point.

* Variables are specified in conf.properties. Use this file to change the variables if required. The two variables should be separated by ", ". It can be changed but replace the delimiter in pagination.R file.

* Mongodb database configurations can be changed in conf.properties file.

* Integration of R with java requires Rserver to be on. It can be done by:

For windows:
```
#!R

Rserve()
```

For Linux:

```
#!R

Rserve(args='--no-save')
```


* For downloading the browser for the first time, use

```
#!R

rD <- rsDriver(port = 4567L, browser = c("chrome", "firefox", "phantomjs", "internet explorer"), version = "latest", chromever = "latest", geckover = "latest", iedrver = NULL, phantomver = "2.1.1", verbose = TRUE, check = TRUE)
```


* getlatlon function is used to get latitude and longitude. It is to be replaced with local Latitude-Longitude API of GPSDESK.

* Chrome is to be replaced with phantomJS for headless browsing (background).