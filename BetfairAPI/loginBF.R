loginBF <- function(username, password, applicationKey, sslVerify = TRUE){

    credentials <- paste("username=",username,"&password=",password,sep="")

    headersLogin <- list('Accept' = 'application/json', 'X-Application' = applicationKey)

    loginReturn <- RCurl::postForm("https://identitysso.betfair.com/api/login", .opts=list(postfields=credentials, httpheader=headersLogin, ssl.verifypeer = sslVerify))

    authenticationKey <- jsonlite::fromJSON(loginReturn)

    # Assigning a global variable with <<-, I'm giddy with mischievious excitement

    headersPostLogin <- NULL

    headersPostLogin <<- list('Accept' = 'application/json', 'X-Application' = authenticationKey$product, 'X-Authentication' = authenticationKey$token, 'Content-Type' = 'application/json')

}