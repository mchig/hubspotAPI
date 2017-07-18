#recebe dataframe in which, each line is a contact with at least an "email"
#open contact with the Hubspot API and create the contact
addContactHub <- function(contacts){
  x<-vector()
  for (i in 1:dim(contacts)[1]){
    x[i]<-addoneContactHub(contacts[i,])
    print(paste("Status Code: ",x[i]," - ","Email: ",contacts[i,1],sep=""))
  }
  contacts$status_code<-x
  return(contacts)
}

addoneContactHub <- function(contacts){
  APIKEY_VALUE <- "ca854c74-2196-4c5a-8756-50055f0bf53b"
  xurl <- "https://api.hubapi.com/contacts/v1/contact/?hapikey="
  url <- paste(xurl, APIKEY_VALUE,sep="")
  
  validcontact<-data.frame(matrix(data=NA,nrow = dim(contacts)[2],ncol = 2))
  
  names(validcontact)<- c("property","value")
  validcontact$property<-names(contacts)
  
  for (i in 1:dim(contacts)[2]){
    validcontact[i,2]<-contacts[[1,i]]
  }
    
  body <- list()
  body$properties<-validcontact
  
  res <- POST(url=url,
            body=body,
            encode="json")
  
  return(res$status_code)
}