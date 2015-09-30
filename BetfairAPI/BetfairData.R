# https://github.com/phillc73/abettor/blob/master/vignettes/abettor-placeBet.Rmd

require(RCurl)
require(jsonlite)
require(abettor)

sslVerifyOption <- FALSE
sport<- "5"

#options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

# log in to betfair
loginBF(username="dgwparenti@gmail.com",
        password="6nations",
        applicationKey = "43XX5Vi3ZmHCytFd",
        sslVerify = sslVerifyOption)
# Find Relevant Event Type
listEventTypes(sslVerify = sslVerifyOption)

# Find Relevant Country Code
listCountries(eventTypeIds = sport, sslVerify = sslVerifyOption)

# Find Relevant Market Type
listMarketTypes(eventTypeIds = sport, sslVerify = sslVerifyOption)

# Find Market and Selection IDs
marketCat <- listMarketCatalogue(eventTypeIds = sport,
                                 marketCountries = "GB",
                                 marketTypeCodes = "HANDICAP",
                                 sslVerify = sslVerifyOption)
