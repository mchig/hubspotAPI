#recebe dataframe in which, each line is a contact with at least an "email"
#open contact with the Hubspot API and create the contact
updateContactsHub <- function(contacts){
  x<-vector()
  for (i in 1:dim(contacts)[1]){
    x[i]<-updateOneContactHub(contacts[i,])
    print(paste("Status Code: ",x[i]," - ","Email: ",contacts[i,1],sep=""))
  }
  contacts$status_code<-x
  return(contacts)
}

updateOneContactHub <- function(contacts){
  APIKEY_VALUE <- "ca854c74-2196-4c5a-8756-50055f0bf53b"
  xurl <- paste("https://api.hubapi.com/contacts/v1/contact/email/",contacts$email,"/profile?hapikey=",sep="")
  url <- paste(xurl, APIKEY_VALUE,sep="")
  
  validcontact<-data.frame(matrix(data=NA,nrow = dim(contacts)[2],ncol = 2))
  
  names(validcontact)<- c("property","value")
  validcontact$property<-names(contacts)
  
  for (i in 1:dim(contacts)[2]){
    validcontact[i,2]<-contacts[[1,i]]
  }
  
  validcontact <- validcontact[-(validcontact$property=="email"),]
  
  body <- list()
  body$properties<-validcontact
  
  res <- POST(url=url,
              body=body,
              encode="json")
  
  return(res$status_code)
}