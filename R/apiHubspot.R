library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(grid)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(zoo)
library(reshape2)
library(tidyr)

##a mais
library(splitstackshape)
library(tm)
#library(SnowballCC)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(ggplot2)
library(scales)
library(slam)

#recebe lista de contatos do hubspot e retorna tabela com propriedades a partir disto
toTable <- function(APIKEY,hubscontent){
    colunas = 43
    final<- data.frame(matrix(ncol=colunas))
    names(final)<-c("email","account_id","lead_source_geral_new","lead_source_new","hubspot_owner_id","opp_new_date","lifecyclestage","deal_stage","became_customer_new"
                    ,"lost_reason","tpv_novo_comercial","condition","faturamento_grupo","reuniao_date","prim_contato_date","qualificado_date","prop_enviada_date"
                    ,"prop_aceita_date","em_integracao_date","integrado_date","convertido_date","nao_e_pra_agora_date","perdido_date","hs_lifecyclestage_lead_date"
                    ,"hs_lifecyclestage_marketingqualifiedlead_date","ecommerce_platform","hubspotscore","receita_ano","receita_mes","receita_total","total_tpv"
                    ,"tpv_ano","tpv_anteontem","tpv_ontem","tpv_max","tpv_mes","tpv_mes_1","tpv_mes_2","tpv_mes_3","monthly_revenue","resposta_inicial","lastmodifieddate","createdate")
    for(i in 1:length(hubscontent)){
        final[i,]<-NA
        final[i,1] <- ifelse(is.null(hubscontent[[i]]$properties$email$value),NA,hubscontent[[i]]$properties$email$value)
        final[i,2] <- ifelse(is.null(hubscontent[[i]]$properties$account_id$value),NA,as.numeric(hubscontent[[i]]$properties$account_id$value))
        final[i,3] <- ifelse(is.null(hubscontent[[i]]$properties$lead_source_geral_new$value),NA,hubscontent[[i]]$properties$lead_source_geral_new$value)
        final[i,4] <- ifelse(is.null(hubscontent[[i]]$properties$lead_source_new$value),NA,hubscontent[[i]]$properties$lead_source_new$value)
        final[i,5] <- ifelse(is.null(hubscontent[[i]]$properties$hubspot_owner_id$value),NA,hubscontent[[i]]$properties$hubspot_owner_id$value)
        final[i,6] <- ifelse(is.null(hubscontent[[i]]$properties$opp_new_date$value),NA,hubscontent[[i]]$properties$opp_new_date$value)
        final[i,7] <- ifelse(is.null(hubscontent[[i]]$properties$lifecyclestage$value),NA,hubscontent[[i]]$properties$lifecyclestage$value)
        final[i,8] <- ifelse(is.null(hubscontent[[i]]$properties$deal_stage$value),NA,hubscontent[[i]]$properties$deal_stage$value)
        final[i,9] <- ifelse(is.null(hubscontent[[i]]$properties$became_customer_new$value),NA,hubscontent[[i]]$properties$became_customer_new$value)
        final[i,10] <- ifelse(is.null(hubscontent[[i]]$properties$lost_reason$value),NA,hubscontent[[i]]$properties$lost_reason$value)
        final[i,11] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_novo_comercial$value),NA,hubscontent[[i]]$properties$tpv_novo_comercial$value)
        final[i,12] <- ifelse(is.null(hubscontent[[i]]$properties$condition$value),NA,hubscontent[[i]]$properties$condition$value)
        final[i,13] <- ifelse(is.null(hubscontent[[i]]$properties$faturamento_grupo$value),NA,hubscontent[[i]]$properties$faturamento_grupo$value)
        final[i,14] <- ifelse(is.null(hubscontent[[i]]$properties$reuniao_date$value),NA,hubscontent[[i]]$properties$reuniao_date$value)
        final[i,15] <- ifelse(is.null(hubscontent[[i]]$properties$prim_contato_date$value),NA,hubscontent[[i]]$properties$prim_contato_date$value)
        final[i,16] <- ifelse(is.null(hubscontent[[i]]$properties$qualificado_date$value),NA,hubscontent[[i]]$properties$qualificado_date$value)
        final[i,17] <- ifelse(is.null(hubscontent[[i]]$properties$prop_enviada_date$value),NA,hubscontent[[i]]$properties$prop_enviada_date$value)
        final[i,18] <- ifelse(is.null(hubscontent[[i]]$properties$prop_aceita_date$value),NA,hubscontent[[i]]$properties$prop_aceita_date$value)
        final[i,19] <- ifelse(is.null(hubscontent[[i]]$properties$em_integracao_date$value),NA,hubscontent[[i]]$properties$em_integracao_date$value)
        final[i,20] <- ifelse(is.null(hubscontent[[i]]$properties$integrado_date$value),NA,hubscontent[[i]]$properties$integrado_date$value)
        final[i,21] <- ifelse(is.null(hubscontent[[i]]$properties$convertido_date$value),NA,hubscontent[[i]]$properties$convertido_date$value)
        final[i,22] <- ifelse(is.null(hubscontent[[i]]$properties$nao_e_pra_agora_date$value),NA,hubscontent[[i]]$properties$nao_e_pra_agora_date$value)
        final[i,23] <- ifelse(is.null(hubscontent[[i]]$properties$perdido_date$value),NA,hubscontent[[i]]$properties$perdido_date$value)
        final[i,24] <- ifelse(is.null(hubscontent[[i]]$properties$hs_lifecyclestage_lead_date$value),NA,hubscontent[[i]]$properties$hs_lifecyclestage_lead_date$value)
        final[i,25] <- ifelse(is.null(hubscontent[[i]]$properties$hs_lifecyclestage_marketingqualifiedlead_date$value),NA,hubscontent[[i]]$properties$hs_lifecyclestage_marketingqualifiedlead_date$value)
        final[i,26] <- ifelse(is.null(hubscontent[[i]]$properties$ecommerce_platform$value),NA,hubscontent[[i]]$properties$ecommerce_platform$value)
        final[i,27] <- ifelse(is.null(hubscontent[[i]]$properties$hubspotscore$value),NA,as.numeric(hubscontent[[i]]$properties$hubspotscore$value))
        final[i,28] <- ifelse(is.null(hubscontent[[i]]$properties$receita_ano$value),NA,as.numeric(hubscontent[[i]]$properties$receita_ano$value))
        final[i,29] <- ifelse(is.null(hubscontent[[i]]$properties$receita_mes$value),NA,as.numeric(hubscontent[[i]]$properties$receita_mes$value))
        final[i,30] <- ifelse(is.null(hubscontent[[i]]$properties$receita_total$value),NA,as.numeric(hubscontent[[i]]$properties$receita_total$value))
        final[i,31] <- ifelse(is.null(hubscontent[[i]]$properties$total_tpv$value),NA,as.numeric(hubscontent[[i]]$properties$total_tpv$value))
        final[i,32] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_ano$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_ano$value))
        final[i,33] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_anteontem$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_anteontem$value))
        final[i,34] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_ontem$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_ontem$value))
        final[i,35] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_max$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_max$value))
        final[i,36] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_mes$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_mes$value))
        final[i,37] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_mes_1$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_mes_1$value))
        final[i,38] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_mes_2$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_mes_2$value))
        final[i,39] <- ifelse(is.null(hubscontent[[i]]$properties$tpv_mes_3$value),NA,as.numeric(hubscontent[[i]]$properties$tpv_mes_3$value))
        final[i,40] <- ifelse(is.null(hubscontent[[i]]$properties$monthly_revenue$value),NA,hubscontent[[i]]$properties$monthly_revenue$value)
        final[i,41] <- ifelse(is.null(hubscontent[[i]]$properties$resposta_inicial$value),NA,hubscontent[[i]]$properties$resposta_inicial$value)
        final[i,42] <- ifelse(is.null(hubscontent[[i]]$properties$lastmodifieddate$value),NA,hubscontent[[i]]$properties$lastmodifieddate$value)
        final[i,43] <- ifelse(is.null(hubscontent[[i]]$properties$createdate$value),NA,hubscontent[[i]]$properties$createdate$value)
    }
    #add owners name and email
    owners <- getOwners(APIKEY)
    final <- merge(final, owners, by = "hubspot_owner_id", all.x = TRUE)

    #arrange the dates
    final$opp_new_date<-as.Date(as.POSIXlt(as.numeric(final$opp_new_date)/1000, origin="1970-01-01"))
    final$became_customer_new<-as.Date(as.POSIXlt(as.numeric(final$became_customer_new)/1000, origin="1970-01-01"))
    final$reuniao_date<-as.Date(as.POSIXlt(as.numeric(final$reuniao_date)/1000, origin="1970-01-01"))
    final$prim_contato_date<-as.Date(as.POSIXlt(as.numeric(final$prim_contato_date)/1000, origin="1970-01-01"))
    final$qualificado_date<-as.Date(as.POSIXlt(as.numeric(final$qualificado_date)/1000, origin="1970-01-01"))
    final$prop_enviada_date<-as.Date(as.POSIXlt(as.numeric(final$prop_enviada_date)/1000, origin="1970-01-01"))
    final$prop_aceita_date<-as.Date(as.POSIXlt(as.numeric(final$prop_aceita_date)/1000, origin="1970-01-01"))
    final$em_integracao_date<-as.Date(as.POSIXlt(as.numeric(final$em_integracao_date)/1000, origin="1970-01-01"))
    final$integrado_date<-as.Date(as.POSIXlt(as.numeric(final$integrado_date)/1000, origin="1970-01-01"))
    final$convertido_date<-as.Date(as.POSIXlt(as.numeric(final$convertido_date)/1000, origin="1970-01-01"))
    final$nao_e_pra_agora_date<-as.Date(as.POSIXlt(as.numeric(final$nao_e_pra_agora_date)/1000, origin="1970-01-01"))
    final$perdido_date<-as.Date(as.POSIXlt(as.numeric(final$perdido_date)/1000, origin="1970-01-01"))
    final$hs_lifecyclestage_lead_date<-as.Date(as.POSIXlt(as.numeric(final$hs_lifecyclestage_lead_date)/1000, origin="1970-01-01"))
    final$hs_lifecyclestage_marketingqualifiedlead_date<-as.Date(as.POSIXlt(as.numeric(final$hs_lifecyclestage_marketingqualifiedlead_date)/1000, origin="1970-01-01"))
    final$lastmodifieddate<-as.Date(as.POSIXlt(as.numeric(final$lastmodifieddate)/1000, origin="1970-01-01"))
    final$createdate<-as.Date(as.POSIXlt(as.numeric(final$createdate)/1000, origin="1970-01-01"))

    return(final)
}
#retorna tabela de IDs do hubspot com seus respectivos owners e emails
getOwners <- function(APIKEY){
    APIKEY_VALUE <- APIKEY
    HS_API_URL <- "http://api.hubapi.com"
    APIKEY <- paste("?hapikey=", APIKEY_VALUE,sep="")
    xulr <-"/owners/v2/owners/"

    url <- paste(HS_API_URL, xulr, APIKEY,sep="")

    #raw.result <- GET(url = url)
    #this.raw.content <- rawToChar(raw.result$content)
    this.raw.content <- content(GET(url = url), "text")
    this.content <- fromJSON(this.raw.content)

    hubspot_owner_id<-this.content$ownerId
    hubspot_owner_name<-paste(this.content$firstName,this.content$lastName)
    hubspot_owner_email<-this.content$email
    owners<-data.frame(hubspot_owner_id,hubspot_owner_name,hubspot_owner_email)
    return(owners)
}
#arruma data de contatos
arrangeDate <- function(contatosHub){

    #garantir que tds contatos com data tenham opp data
    min <- as.Date(apply(contatosHub[,c(which(names(contatosHub)=="prim_contato_date"),which(names(contatosHub)=="qualificado_date"),which(names(contatosHub)=="prop_enviada_date"),
                                        which(names(contatosHub)=="prop_aceita_date"),which(names(contatosHub)=="em_integracao_date"),which(names(contatosHub)=="integrado_date"),
                                        which(names(contatosHub)=="convertido_date"),
                                        which(names(contatosHub)=="nao_e_pra_agora_date"),
                                        which(names(contatosHub)=="perdido_date"))], 1, min,na.rm = TRUE))
    contatosHub$opp_new_date[is.na(contatosHub$reuniao_date)  & is.na(contatosHub$opp_new_date) & !is.na(min)] <- min [is.na(contatosHub$reuniao_date)
                                                                                                                       & is.na(contatosHub$opp_new_date) & !is.na(min)]

    #garantir que reuniao seja maior entre reuniao e opp
    contatosHub$reuniao_date <- as.Date(apply(contatosHub[,c(which(names(contatosHub)=="opp_new_date"),which(names(contatosHub)=="reuniao_date"))],
                                              1, max,na.rm = TRUE))

    #colocar NA em contatos com #convertido# date menor que reuniao
    contatosHub$convertido_date[!is.na(contatosHub$reuniao_date) &
                                    !is.na(contatosHub$convertido_date) &
                                    contatosHub$reuniao_date>contatosHub$convertido_date] <- NA
    contatosHub$convertido_date[!is.na(contatosHub$prim_contato_date) &
                                  !is.na(contatosHub$convertido_date) &
                                  contatosHub$prim_contato_date>contatosHub$convertido_date] <- NA
    contatosHub$convertido_date[!is.na(contatosHub$qualificado_date) &
                                  !is.na(contatosHub$convertido_date) &
                                  contatosHub$qualificado_date>contatosHub$convertido_date] <- NA
    contatosHub$convertido_date[!is.na(contatosHub$prop_enviada_date) &
                                  !is.na(contatosHub$convertido_date) &
                                  contatosHub$prop_enviada_date>contatosHub$convertido_date] <- NA
    contatosHub$convertido_date[!is.na(contatosHub$prop_aceita_date) &
                                  !is.na(contatosHub$convertido_date) &
                                  contatosHub$prop_aceita_date>contatosHub$convertido_date] <- NA
    contatosHub$convertido_date[!is.na(contatosHub$em_integracao_date) &
                                  !is.na(contatosHub$convertido_date) &
                                  contatosHub$em_integracao_date>contatosHub$convertido_date] <- NA
    contatosHub$convertido_date[!is.na(contatosHub$integrado_date) &
                                  !is.na(contatosHub$convertido_date) &
                                  contatosHub$integrado_date>contatosHub$convertido_date] <- NA


    #colocar NA em contatos com #integrado# date menor que reuniao e colocar a data de convertido em integrado com NA mas com convertido date
    contatosHub$integrado_date[!is.na(contatosHub$reuniao_date) &
                                   !is.na(contatosHub$integrado_date) &
                                   contatosHub$reuniao_date>contatosHub$integrado_date] <- NA
    contatosHub$integrado_date[!is.na(contatosHub$prim_contato_date) &
                                 !is.na(contatosHub$integrado_date) &
                                 contatosHub$prim_contato_date>contatosHub$integrado_date] <- NA
    contatosHub$integrado_date[!is.na(contatosHub$qualificado_date) &
                                 !is.na(contatosHub$integrado_date) &
                                 contatosHub$qualificado_date>contatosHub$integrado_date] <- NA
    contatosHub$integrado_date[!is.na(contatosHub$prop_enviada_date) &
                                 !is.na(contatosHub$integrado_date) &
                                 contatosHub$prop_enviada_date>contatosHub$integrado_date] <- NA
    contatosHub$integrado_date[!is.na(contatosHub$prop_aceita_date) &
                                 !is.na(contatosHub$integrado_date) &
                                 contatosHub$prop_aceita_date>contatosHub$integrado_date] <- NA
    contatosHub$integrado_date[!is.na(contatosHub$em_integracao_date) &
                                 !is.na(contatosHub$integrado_date) &
                                 contatosHub$em_integracao_date>contatosHub$integrado_date] <- NA
    contatosHub$integrado_date[is.na(contatosHub$integrado_date) &
                                   !is.na(contatosHub$convertido_date)] <- contatosHub$convertido_date[is.na(contatosHub$integrado_date) &
                                                                                                           !is.na(contatosHub$convertido_date)]
    #repetir para em integracao
    contatosHub$em_integracao_date[!is.na(contatosHub$reuniao_date) &
                                       !is.na(contatosHub$em_integracao_date) &
                                       contatosHub$reuniao_date>contatosHub$em_integracao_date] <- NA
    contatosHub$em_integracao_date[!is.na(contatosHub$prim_contato_date) &
                                     !is.na(contatosHub$em_integracao_date) &
                                     contatosHub$prim_contato_date>contatosHub$em_integracao_date] <- NA
    contatosHub$em_integracao_date[!is.na(contatosHub$qualificado_date) &
                                     !is.na(contatosHub$em_integracao_date) &
                                     contatosHub$qualificado_date>contatosHub$em_integracao_date] <- NA
    contatosHub$em_integracao_date[!is.na(contatosHub$prop_enviada_date) &
                                     !is.na(contatosHub$em_integracao_date) &
                                     contatosHub$prop_enviada_date>contatosHub$em_integracao_date] <- NA
    contatosHub$em_integracao_date[!is.na(contatosHub$prop_aceita_date) &
                                     !is.na(contatosHub$em_integracao_date) &
                                     contatosHub$prop_aceita_date>contatosHub$em_integracao_date] <- NA
    contatosHub$em_integracao_date[is.na(contatosHub$em_integracao_date) &
                                       !is.na(contatosHub$integrado_date)] <- contatosHub$integrado_date[is.na(contatosHub$em_integracao_date) &
                                                                                                             !is.na(contatosHub$integrado_date)]
    #repetir para em prop aceita
    contatosHub$prop_aceita_date[!is.na(contatosHub$reuniao_date) &
                                     !is.na(contatosHub$prop_aceita_date) &
                                     contatosHub$reuniao_date>contatosHub$prop_aceita_date] <- NA
    contatosHub$prop_aceita_date[!is.na(contatosHub$prim_contato_date) &
                                   !is.na(contatosHub$prop_aceita_date) &
                                   contatosHub$prim_contato_date>contatosHub$prop_aceita_date] <- NA
    contatosHub$prop_aceita_date[!is.na(contatosHub$qualificado_date) &
                                   !is.na(contatosHub$prop_aceita_date) &
                                   contatosHub$qualificado_date>contatosHub$prop_aceita_date] <- NA
    contatosHub$prop_aceita_date[!is.na(contatosHub$prop_enviada_date) &
                                   !is.na(contatosHub$prop_aceita_date) &
                                   contatosHub$prop_enviada_date>contatosHub$prop_aceita_date] <- NA
    contatosHub$prop_aceita_date[is.na(contatosHub$prop_aceita_date) &
                                     !is.na(contatosHub$em_integracao_date)] <- contatosHub$em_integracao_date[is.na(contatosHub$prop_aceita_date) &
                                                                                                                   !is.na(contatosHub$em_integracao_date)]

    #repetir para em prop enviada
    contatosHub$prop_enviada_date[!is.na(contatosHub$reuniao_date) &
                                      !is.na(contatosHub$prop_enviada_date) &
                                      contatosHub$reuniao_date>contatosHub$prop_enviada_date] <- NA
    contatosHub$prop_enviada_date[!is.na(contatosHub$prim_contato_date) &
                                    !is.na(contatosHub$prop_enviada_date) &
                                    contatosHub$prim_contato_date>contatosHub$prop_enviada_date] <- NA
    contatosHub$prop_enviada_date[!is.na(contatosHub$qualificado_date) &
                                    !is.na(contatosHub$prop_enviada_date) &
                                    contatosHub$qualificado_date>contatosHub$prop_enviada_date] <- NA

    contatosHub$prop_enviada_date[is.na(contatosHub$prop_enviada_date) &
                                      !is.na(contatosHub$prop_aceita_date)] <- contatosHub$prop_aceita_date[is.na(contatosHub$prop_enviada_date) &
                                                                                                                !is.na(contatosHub$prop_aceita_date)]
    #repetir para qualificado
    contatosHub$qualificado_date[!is.na(contatosHub$reuniao_date) &
                                     !is.na(contatosHub$qualificado_date) &
                                     contatosHub$reuniao_date>contatosHub$qualificado_date] <- NA
    contatosHub$qualificado_date[!is.na(contatosHub$prim_contato_date) &
                                   !is.na(contatosHub$qualificado_date) &
                                   contatosHub$prim_contato_date>contatosHub$qualificado_date] <- NA

    contatosHub$qualificado_date[is.na(contatosHub$qualificado_date) &
                                     !is.na(contatosHub$prop_enviada_date)] <- contatosHub$prop_enviada_date[is.na(contatosHub$qualificado_date) &
                                                                                                                 !is.na(contatosHub$prop_enviada_date)]

    #repetir para prim contato
    contatosHub$prim_contato_date[!is.na(contatosHub$reuniao_date) &
                                      !is.na(contatosHub$prim_contato_date) &
                                      contatosHub$reuniao_date>contatosHub$prim_contato_date] <- NA
    contatosHub$prim_contato_date[is.na(contatosHub$prim_contato_date) &
                                      !is.na(contatosHub$qualificado_date)] <- contatosHub$qualificado_date[is.na(contatosHub$prim_contato_date) &
                                                                                                                                                                                                               !is.na(contatosHub$qualificado_date)]
    return(contatosHub)
}
#retorna lista de contatos do Hubspot a partir de uma ID de lista
getHubspotContacts <- function(APIKEY, listID="2133"){
    ###############################
    ###############################
    #API key
    APIKEY_VALUE <- APIKEY
    HS_API_URL <- "http://api.hubapi.com"

    ###############################
    ###############################
    #get all vIDs from list id


    has.more=TRUE
    NMAX = 400
    n=1
    vidOffset=0
    vids<-list()
    contactslist<-list()

    while(has.more==TRUE & n<=NMAX){

        #pega vIDs
        APIKEY <- paste("?hapikey=", APIKEY_VALUE,"&count=250&vidOffset=",vidOffset,sep="")
        xulr <- paste("/contacts/v1/lists/",listID,"/contacts/all",sep="")

        url <- paste(HS_API_URL, xulr, APIKEY,sep="")

        this.raw.content <- content(GET(url = url), "text")
        this.content <- fromJSON(this.raw.content)

        vidOffset<-this.content$`vid-offset`
        has.more<- this.content$`has-more`

        vids[[n]]<-this.content$contacts$vid
        vec<-unlist(vids[[n]])

        #pega Contatos
        vidsV<-paste("vid=",vec,sep="",collapse = "&")
        xulr <-"/contacts/v1/contact/vids/batch/?"

        APIKEY <- paste("&hapikey=", APIKEY_VALUE,sep="")

        url <- paste(HS_API_URL, xulr, vidsV, APIKEY,sep="")

        this.raw.content <- content(GET(url = url), "text")
        this.content <- fromJSON(this.raw.content)

        contactslist[[n]]<-toTable(APIKEY = APIKEY_VALUE, hubscontent = this.content)

        n<-n+1
    }

    contactsFunil <- ldply(contactslist, data.frame)

    return (contactsFunil)
}
#cria tabela com numeros em cada etapa do funil
criaFunilComercial <- function (contatos, vendedor = "all", lead_source_geral = "all", lead_source = "all", type = "safra", ecommerce_platform = "all",
                                dataRef = NULL, Q = NULL, dataFinal = NULL){
    ## set vectors for all option ##
    if (vendedor == "all"){
        vendedor <- unique(contatos$hubspot_owner_name)
    }
    if (lead_source_geral == "all"){
        lead_source_geral <- unique(contatos$lead_source_geral_new)
    }
    if (lead_source == "all"){
        lead_source <- unique(contatos$lead_source_new)
    }
    if (ecommerce_platform == "all"){
        ecommerce_platform <- unique(contatos$ecommerce_platform)
    }
    #arrange contacts dates
    #contatosD <- suppressWarnings(arrangeDate (contatos))
    contatosD <- contatos
    ## visao do mes ##
    if (type == "mes"){
        if (is.null(dataRef)){
            dataRef <- as.Date(paste(year(now()),"-",month(now()),"-01",sep=""))
        }else{
            if (is.null(dataFinal)){
                dataFinal <- as.Date(paste(year(dataRef),"-",month(dataRef)+1,"-01",sep=""))
            }
            #dataRef <- as.Date(paste(year(dataRef),"-",month(dataRef),"-01",sep=""))
        }

        reuniao <- length(which(!is.na(contatosD$reuniao_date) &
                                    (contatosD$reuniao_date>=dataRef) &
                                    (contatosD$reuniao_date<=dataFinal) &
                                    contatosD$hubspot_owner_name%in%vendedor &
                                    contatosD$lead_source_geral_new%in%lead_source_geral &
                                    contatosD$lead_source_new%in%lead_source &
                                    contatosD$ecommerce_platform%in%ecommerce_platform))

        pcontato <- length(which(!is.na(contatosD$prim_contato_date) &
                                     (contatosD$prim_contato_date>=dataRef) &
                                     (contatosD$prim_contato_date<=dataFinal) &
                                     contatosD$hubspot_owner_name%in%vendedor &
                                     contatosD$lead_source_geral_new%in%lead_source_geral &
                                     contatosD$lead_source_new%in%lead_source &
                                     contatosD$ecommerce_platform%in%ecommerce_platform))

        qualificado <- length(which(!is.na(contatosD$qualificado_date) &
                                        (contatosD$qualificado_date>=dataRef) &
                                        (contatosD$qualificado_date<=dataFinal) &
                                        contatosD$hubspot_owner_name%in%vendedor &
                                        contatosD$lead_source_geral_new%in%lead_source_geral &
                                        contatosD$lead_source_new%in%lead_source &
                                        contatosD$ecommerce_platform%in%ecommerce_platform))

        penviada <- length(which(!is.na(contatosD$prop_enviada_date) &
                                     (contatosD$prop_enviada_date>=dataRef) &
                                     (contatosD$prop_enviada_date<=dataFinal) &
                                     contatosD$hubspot_owner_name%in%vendedor &
                                     contatosD$lead_source_geral_new%in%lead_source_geral &
                                     contatosD$lead_source_new%in%lead_source &
                                     contatosD$ecommerce_platform%in%ecommerce_platform))

        paceita <- length(which(!is.na(contatosD$prop_aceita_date) &
                                    (contatosD$prop_aceita_date>=dataRef) &
                                    (contatosD$prop_aceita_date<=dataFinal) &
                                    contatosD$hubspot_owner_name%in%vendedor &
                                    contatosD$lead_source_geral_new%in%lead_source_geral &
                                    contatosD$lead_source_new%in%lead_source &
                                    contatosD$ecommerce_platform%in%ecommerce_platform))

        emintegracao <- length(which(!is.na(contatosD$em_integracao_date) &
                                         (contatosD$em_integracao_date>=dataRef) &
                                         (contatosD$em_integracao_date<=dataFinal) &
                                         contatosD$hubspot_owner_name%in%vendedor &
                                         contatosD$lead_source_geral_new%in%lead_source_geral &
                                         contatosD$lead_source_new%in%lead_source &
                                         contatosD$ecommerce_platform%in%ecommerce_platform))

        integrado <- length(which(!is.na(contatosD$integrado_date) &
                                      (contatosD$integrado_date>=dataRef) &
                                      (contatosD$integrado_date<=dataFinal) &
                                      contatosD$hubspot_owner_name%in%vendedor &
                                      contatosD$lead_source_geral_new%in%lead_source_geral &
                                      contatosD$lead_source_new%in%lead_source &
                                      contatosD$ecommerce_platform%in%ecommerce_platform))

        convertido <- length(which(!is.na(contatosD$convertido_date) &
                                       (contatosD$convertido_date>=dataRef) &
                                       (contatosD$convertido_date<=dataFinal) &
                                       contatosD$hubspot_owner_name%in%vendedor &
                                       contatosD$lead_source_geral_new%in%lead_source_geral &
                                       contatosD$lead_source_new%in%lead_source &
                                       contatosD$ecommerce_platform%in%ecommerce_platform &
                                       contatos$deal_stage=="Convertido"))
    }
    ## visao de trimestral ##
    else if (type == "tri"){

        if (is.null(dataRef)){
            if (is.null(Q)){
                Q <- quarter(now())
            }
            if (Q == 1){
                dataRef <- as.Date(paste(year(now()),"-01-01",sep=""))
            }else if (Q == 2){
                dataRef <- as.Date(paste(year(now()),"-04-01",sep=""))
            }else if (Q == 3){
                dataRef <- as.Date(paste(year(now()),"-07-01",sep=""))
            }else {
                dataRef <- as.Date(paste(year(now()),"-10-01",sep=""))
            }
        }
        else if (!is.null(dataRef)){
            if (is.null(Q)){
               Q <- quarter(dataRef)
            }
            if (Q == 1){
                dataRef <- as.Date(paste(year(dataRef),"-01-01",sep=""))
            }else if (Q == 2){
                dataRef <- as.Date(paste(year(dataRef),"-04-01",sep=""))
            }else if (Q == 3){
                dataRef <- as.Date(paste(year(dataRef),"-07-01",sep=""))
            }else {
                dataRef <- as.Date(paste(year(dataRef),"-10-01",sep=""))
            }
        }

        dataFinal <- dataRef
        month(dataFinal) <- month(dataRef) + 2
        day(dataFinal) <- days_in_month(dataFinal)

        reuniao <- length(which(!is.na(contatosD$reuniao_date) &
                                    (contatosD$reuniao_date>=dataRef) &
                                    (contatosD$reuniao_date<=dataFinal) &
                                    contatosD$hubspot_owner_name%in%vendedor &
                                    contatosD$lead_source_geral_new%in%lead_source_geral &
                                    contatosD$lead_source_new%in%lead_source &
                                    contatosD$ecommerce_platform%in%ecommerce_platform))

        pcontato <- length(which(!is.na(contatosD$prim_contato_date) &
                                     (contatosD$prim_contato_date>=dataRef) &
                                     (contatosD$prim_contato_date<=dataFinal) &
                                     contatosD$hubspot_owner_name%in%vendedor &
                                     contatosD$lead_source_geral_new%in%lead_source_geral &
                                     contatosD$lead_source_new%in%lead_source &
                                     contatosD$ecommerce_platform%in%ecommerce_platform))

        qualificado <- length(which(!is.na(contatosD$qualificado_date) &
                                        (contatosD$qualificado_date>=dataRef) &
                                        (contatosD$qualificado_date<=dataFinal) &
                                        contatosD$hubspot_owner_name%in%vendedor &
                                        contatosD$lead_source_geral_new%in%lead_source_geral &
                                        contatosD$lead_source_new%in%lead_source &
                                        contatosD$ecommerce_platform%in%ecommerce_platform))

        penviada <- length(which(!is.na(contatosD$prop_enviada_date) &
                                     (contatosD$prop_enviada_date>=dataRef) &
                                     (contatosD$prop_enviada_date<=dataFinal) &
                                     contatosD$hubspot_owner_name%in%vendedor &
                                     contatosD$lead_source_geral_new%in%lead_source_geral &
                                     contatosD$lead_source_new%in%lead_source &
                                     contatosD$ecommerce_platform%in%ecommerce_platform))

        paceita <- length(which(!is.na(contatosD$prop_aceita_date) &
                                    (contatosD$prop_aceita_date>=dataRef) &
                                    (contatosD$prop_aceita_date<=dataFinal) &
                                    contatosD$hubspot_owner_name%in%vendedor &
                                    contatosD$lead_source_geral_new%in%lead_source_geral &
                                    contatosD$lead_source_new%in%lead_source &
                                    contatosD$ecommerce_platform%in%ecommerce_platform))

        emintegracao <- length(which(!is.na(contatosD$em_integracao_date) &
                                         (contatosD$em_integracao_date>=dataRef) &
                                         (contatosD$em_integracao_date<=dataFinal) &
                                         contatosD$hubspot_owner_name%in%vendedor &
                                         contatosD$lead_source_geral_new%in%lead_source_geral &
                                         contatosD$lead_source_new%in%lead_source &
                                         contatosD$ecommerce_platform%in%ecommerce_platform))

        integrado <- length(which(!is.na(contatosD$integrado_date) &
                                      (contatosD$integrado_date>=dataRef) &
                                      (contatosD$integrado_date<=dataFinal) &
                                      contatosD$hubspot_owner_name%in%vendedor &
                                      contatosD$lead_source_geral_new%in%lead_source_geral &
                                      contatosD$lead_source_new%in%lead_source &
                                      contatosD$ecommerce_platform%in%ecommerce_platform))

        convertido <- length(which(!is.na(contatosD$convertido_date) &
                                       (contatosD$convertido_date>=dataRef) &
                                       (contatosD$convertido_date<=dataFinal) &
                                       contatosD$hubspot_owner_name%in%vendedor &
                                       contatosD$lead_source_geral_new%in%lead_source_geral &
                                       contatosD$lead_source_new%in%lead_source &
                                       contatosD$ecommerce_platform%in%ecommerce_platform &
                                       contatos$deal_stage=="Convertido"))

    }
    ## visao de safra ##
    else {
        if (is.null(dataRef)){
            dataRef <- as.Date(paste(year(now()),"-01-01",sep=""))
        }
        if (is.null(dataFinal)){
          dataFinal <- as.Date(paste(year(dataRef),"-12-31",sep=""))
        }

      reuniao <- length(which(!is.na(contatosD$reuniao_date) &
                                contatosD$reuniao_date>=dataRef &
                                contatosD$reuniao_date<=dataFinal &
                                contatosD$hubspot_owner_name%in%vendedor &
                                contatosD$lead_source_geral_new%in%lead_source_geral &
                                contatosD$lead_source_new%in%lead_source &
                                contatosD$ecommerce_platform%in%ecommerce_platform))

      pcontato <- length(which(!is.na(contatosD$reuniao_date) &
                                 contatosD$reuniao_date>=dataRef &
                                 contatosD$reuniao_date<=dataFinal &
                                 !is.na(contatosD$prim_contato_date) &
                                 contatosD$prim_contato_date>=dataRef &
                                 contatosD$prim_contato_date<=dataFinal &
                                 contatosD$hubspot_owner_name%in%vendedor &
                                 contatosD$lead_source_geral_new%in%lead_source_geral &
                                 contatosD$lead_source_new%in%lead_source &
                                 contatosD$ecommerce_platform%in%ecommerce_platform))

      qualificado <- length(which(!is.na(contatosD$reuniao_date) &
                                    contatosD$reuniao_date>=dataRef &
                                    contatosD$reuniao_date<=dataFinal &
                                    !is.na(contatosD$qualificado_date) &
                                    contatosD$qualificado_date>=dataRef &
                                    contatosD$qualificado_date<=dataFinal &
                                    contatosD$hubspot_owner_name%in%vendedor &
                                    contatosD$lead_source_geral_new%in%lead_source_geral &
                                    contatosD$lead_source_new%in%lead_source &
                                    contatosD$ecommerce_platform%in%ecommerce_platform))

      penviada <- length(which(!is.na(contatosD$reuniao_date) &
                                 contatosD$reuniao_date>=dataRef &
                                 contatosD$reuniao_date<=dataFinal &
                                 !is.na(contatosD$prop_enviada_date) &
                                 contatosD$prop_enviada_date>=dataRef &
                                 contatosD$prop_enviada_date<=dataFinal &
                                 contatosD$hubspot_owner_name%in%vendedor &
                                 contatosD$lead_source_geral_new%in%lead_source_geral &
                                 contatosD$lead_source_new%in%lead_source &
                                 contatosD$ecommerce_platform%in%ecommerce_platform))

      paceita <- length(which(!is.na(contatosD$reuniao_date) &
                                contatosD$reuniao_date>=dataRef &
                                contatosD$reuniao_date<=dataFinal &
                                !is.na(contatosD$prop_aceita_date) &
                                contatosD$prop_aceita_date>=dataRef &
                                contatosD$prop_aceita_date<=dataFinal &
                                contatosD$hubspot_owner_name%in%vendedor &
                                contatosD$lead_source_geral_new%in%lead_source_geral &
                                contatosD$lead_source_new%in%lead_source &
                                contatosD$ecommerce_platform%in%ecommerce_platform))

      emintegracao <- length(which(!is.na(contatosD$reuniao_date) &
                                     contatosD$reuniao_date>=dataRef &
                                     contatosD$reuniao_date<=dataFinal &
                                     !is.na(contatosD$em_integracao_date) &
                                     contatosD$em_integracao_date>=dataRef &
                                     contatosD$em_integracao_date<=dataFinal &
                                     contatosD$hubspot_owner_name%in%vendedor &
                                     contatosD$lead_source_geral_new%in%lead_source_geral &
                                     contatosD$lead_source_new%in%lead_source &
                                     contatosD$ecommerce_platform%in%ecommerce_platform))

      integrado <- length(which(!is.na(contatosD$reuniao_date) &
                                  contatosD$reuniao_date>=dataRef &
                                  contatosD$reuniao_date<=dataFinal &
                                  !is.na(contatosD$integrado_date) &
                                  contatosD$integrado_date>=dataRef &
                                  contatosD$integrado_date<=dataFinal &
                                  contatosD$hubspot_owner_name%in%vendedor &
                                  contatosD$lead_source_geral_new%in%lead_source_geral &
                                  contatosD$lead_source_new%in%lead_source &
                                  contatosD$ecommerce_platform%in%ecommerce_platform))

      convertido <- length(which(!is.na(contatosD$reuniao_date) &
                                   contatosD$reuniao_date>=dataRef &
                                   contatosD$reuniao_date<=dataFinal &
                                   !is.na(contatosD$convertido_date) &
                                   contatosD$convertido_date>=dataRef &
                                   contatosD$convertido_date<=dataFinal &
                                   contatosD$hubspot_owner_name%in%vendedor &
                                   contatosD$lead_source_geral_new%in%lead_source_geral &
                                   contatosD$lead_source_new%in%lead_source &
                                   contatosD$ecommerce_platform%in%ecommerce_platform &
                                   contatos$deal_stage=="Convertido"))
    }

    final <- data.frame(deal_stage = c("Reuniao","Primeiro Contato","Qualificado","Proposta Enviada","Proposta Aceita","Em Integracao","Integrado","Convertido"),
                        ncontacts = c(reuniao,pcontato,qualificado,penviada,paceita,emintegracao,integrado,convertido))

    return (final)
}
#cria grupos de faturamento
criaGrupodeFaturamento <- function (contatos, vendedor = "all", lead_source_geral = "all", lead_source = "all", type = "safra", ecommerce_platform = "all",
                                dataRef = NULL, Q = NULL, dataFinal = NULL){
  ## set vectors for all option ##
  if (vendedor == "all"){
    vendedor <- unique(contatos$hubspot_owner_name)
  }
  if (lead_source_geral == "all"){
    lead_source_geral <- unique(contatos$lead_source_geral_new)
  }
  if (lead_source == "all"){
    lead_source <- unique(contatos$lead_source_new)
  }
  if (ecommerce_platform == "all"){
    ecommerce_platform <- unique(contatos$ecommerce_platform)
  }
  #arrange contacts dates
  #contatosD <- suppressWarnings(arrangeDate (contatos))
  contatosD <- contatos
  ## visao do mes ##
  if (type == "mes"){
    if (is.null(dataRef)){
      dataRef <- as.Date(paste(year(now()),"-",month(now()),"-01",sep=""))
    }else{
        if (is.null(dataFinal)){
            dataFinal <- as.Date(paste(year(dataRef),"-",month(dataRef)+1,"-01",sep=""))
        }
        #dataRef <- as.Date(paste(year(dataRef),"-",month(dataRef),"-01",sep=""))
    }

    F0 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F0") &
                         contatos$deal_stage=="Convertido"))
    F1 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F1") &
                         contatos$deal_stage=="Convertido"))
    F2 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F2") &
                         contatos$deal_stage=="Convertido"))
    F3 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F3") &
                         contatos$deal_stage=="Convertido"))
    F4 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F4") &
                         contatos$deal_stage=="Convertido"))
    F5 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F5") &
                         contatos$deal_stage=="Convertido"))
    F6 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F6") &
                         contatos$deal_stage=="Convertido"))
    F7 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F7") &
                         contatos$deal_stage=="Convertido"))
    ND <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         (is.na(contatosD$faturamento_grupo) |
                         (contatosD$faturamento_grupo=="")) &
                         contatos$deal_stage=="Convertido"))

  }
  ## visao de trimestral ##
  else if (type == "tri"){

    if (is.null(dataRef)){
      if (is.null(Q)){
        Q <- quarter(now())
      }
      if (Q == 1){
        dataRef <- as.Date(paste(year(now()),"-01-01",sep=""))
      }else if (Q == 2){
        dataRef <- as.Date(paste(year(now()),"-04-01",sep=""))
      }else if (Q == 3){
        dataRef <- as.Date(paste(year(now()),"-07-01",sep=""))
      }else {
        dataRef <- as.Date(paste(year(now()),"-10-01",sep=""))
      }
    }
    else if (!is.null(dataRef)){
      if (is.null(Q)){
        Q <- quarter(dataRef)
      }
      if (Q == 1){
        dataRef <- as.Date(paste(year(dataRef),"-01-01",sep=""))
      }else if (Q == 2){
        dataRef <- as.Date(paste(year(dataRef),"-04-01",sep=""))
      }else if (Q == 3){
        dataRef <- as.Date(paste(year(dataRef),"-07-01",sep=""))
      }else {
        dataRef <- as.Date(paste(year(dataRef),"-10-01",sep=""))
      }
    }

    dataFinal <- dataRef
    month(dataFinal) <- month(dataRef) + 2
    day(dataFinal) <- days_in_month(dataFinal)

    F0 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F0" &
                         contatos$deal_stage=="Convertido"))
    F1 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F1" &
                         contatos$deal_stage=="Convertido"))
    F2 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F2" &
                         contatos$deal_stage=="Convertido"))
    F3 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F3" &
                         contatos$deal_stage=="Convertido"))
    F4 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F4" &
                         contatos$deal_stage=="Convertido"))
    F5 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F5" &
                         contatos$deal_stage=="Convertido"))
    F6 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F6" &
                         contatos$deal_stage=="Convertido"))
    F7 <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform&
                         contatosD$faturamento_grupo=="F7" &
                         contatos$deal_stage=="Convertido"))
    ND <- length(which(!is.na(contatosD$convertido_date) &
                         (contatosD$convertido_date>=dataRef) &
                         (contatosD$convertido_date<=dataFinal) &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         (is.na(contatosD$faturamento_grupo) |
                            (contatosD$faturamento_grupo=="")) &
                         contatos$deal_stage=="Convertido"))

  }
  ## visao de safra ##
  else {
    if (is.null(dataRef)){
      dataRef <- as.Date(paste(year(now()),"-01-01",sep=""))
    }
    if (is.null(dataFinal)){
      dataFinal <- as.Date(paste(year(dataRef),"-12-31",sep=""))
    }

    F0 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F0") &
                         contatos$deal_stage=="Convertido"))
    F1 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F1") &
                         contatos$deal_stage=="Convertido"))
    F2 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F2") &
                         contatos$deal_stage=="Convertido"))
    F3 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F3") &
                         contatos$deal_stage=="Convertido"))
    F4 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F4") &
                         contatos$deal_stage=="Convertido"))
    F5 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F5") &
                         contatos$deal_stage=="Convertido"))
    F6 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F6") &
                         contatos$deal_stage=="Convertido"))
    F7 <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         !is.na(contatosD$faturamento_grupo) &
                         (contatosD$faturamento_grupo=="F7") &
                         contatos$deal_stage=="Convertido"))
    ND <- length(which(!is.na(contatosD$reuniao_date) &
                         contatosD$reuniao_date>=dataRef &
                         contatosD$reuniao_date<=dataFinal &
                         !is.na(contatosD$convertido_date) &
                         contatosD$convertido_date>=dataRef &
                         contatosD$convertido_date<=dataFinal &
                         contatosD$hubspot_owner_name%in%vendedor &
                         contatosD$lead_source_geral_new%in%lead_source_geral &
                         contatosD$lead_source_new%in%lead_source &
                         contatosD$ecommerce_platform%in%ecommerce_platform &
                         (is.na(contatosD$faturamento_grupo) |
                            (contatosD$faturamento_grupo=="")) &
                         contatos$deal_stage=="Convertido"))
  }

  final <- data.frame(grupo_faturamento = c("NId","F0","F1","F2","F3","F4","F5","F6","F7"),
                      ncontacts = c(ND,F0,F1,F2,F3,F4,F5,F6,F7))

  return (final)
}
#plota funil
plotFunilComercial <- function(funil, title = "Funil Comercial",subtitle = "", color = "#0000CC", fill = "steelblue", complete = FALSE){
  if (!complete){
    funil <- funil[c(which(funil$deal_stage%in%c("Reuniao","Proposta Enviada","Proposta Aceita","Convertido"))),]
  }
  total <- funil[which(funil$deal_stage=="Reuniao"),which(names(funil)=="ncontacts")]
  funil$padding <- (total - funil$ncontacts) / 2
  funil$efficiency <- 100*funil$ncontacts/total
  molten <- melt(funil[,-c(which(names(funil)=="efficiency"))], id.var='deal_stage')
  molten <- molten[order(molten$variable, decreasing = T), ]
  molten_deal_stage_levels <- c("Convertido","Integrado","Em Integracao","Proposta Aceita","Proposta Enviada","Qualificado","Primeiro Contato","Reuniao")
  molten$deal_stage <- factor(molten$deal_stage, levels = molten_deal_stage_levels)
  funil$deal_stage <- factor(funil$deal_stage, levels = molten_deal_stage_levels)

  p1 <- ggplot(molten, aes(x=deal_stage)) +
    geom_bar(aes(y = value, fill = variable),
             stat='identity', position='stack') +
    geom_text(data=funil,
              aes(y=total/2, label= paste(ncontacts)),
              color='white',size = 8, fontface = "bold") +
    scale_fill_manual(values = c(color, NA) ) +
    coord_flip() +
    theme(legend.position = 'none',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y = element_text(size = 15),
          plot.margin = unit(c(0,0,0,0), "cm")) #cima,direita,baixo,esquerda

  p2 <- ggplot(funil, aes(x=deal_stage, y=efficiency)) +
    geom_bar(stat='identity',fill=fill) +
    geom_text(aes(y = 15, label = paste(round(efficiency), '%',sep="")),
              color="white",size = 7, fontface = "bold") +
    scale_fill_manual(values = c(color)) +
    coord_flip() +
    theme(legend.position = 'none',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(0,0,0,-1), "cm"))


  if (subtitle!=""){
    tg <- textGrob(paste(title," -",subtitle), gp=gpar(fontsize=23))
  }
  else {
    tg <- textGrob(paste(title), gp=gpar(fontsize=23))
  }
  grid.arrange(p1, p2, ncol=2, widths=c(4,1), top=tg)
}
#plota funil
plotGruposdeFaturamento <- function(grupo_faturamento, title = "Grupos de Faturamento", color = "Blues"){
  levels <- c("NId","F0","F1","F2","F3","F4","F5","F6","F7")
  grupo_faturamento$grupo_faturamento <- factor(grupo_faturamento$grupo_faturamento, levels = levels)
  p<-ggplot(grupo_faturamento, aes(x=grupo_faturamento, y=ncontacts, fill=grupo_faturamento)) +
    geom_bar(stat="identity")+scale_fill_brewer(palette=color) +
    geom_text(data=grupo_faturamento,
              aes(y=ncontacts, label= paste(ncontacts),vjust=ifelse(ncontacts>0,1,0)),size = 8,
              fontface = "bold") +
    theme(legend.position = 'none',
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_blank(),
          plot.title = element_text(size=23)) +
    ggtitle(title)
  grid.arrange(p)

}
#filtra contatos
filtraContatos <- function (contatos, vendedor = "all", lead_source_geral = "all", lead_source = "all", type = "safra", ecommerce_platform = "all",
                            dataRef = NULL, Q = NULL, dataFinal = NULL){
  ## set vectors for all option ##
  if (vendedor == "all"){
    vendedor <- unique(contatos$hubspot_owner_name)
  }
  if (lead_source_geral == "all"){
    lead_source_geral <- unique(contatos$lead_source_geral_new)
  }
  if (lead_source == "all"){
    lead_source <- unique(contatos$lead_source_new)
  }
  if (ecommerce_platform == "all"){
    ecommerce_platform <- unique(contatos$ecommerce_platform)
  }
  #arrange contacts dates
  #contatosD <- suppressWarnings(arrangeDate (contatos))
  contatosD <- contatos
  ## visao do mes ##
  if (type == "mes"){
    if (is.null(dataRef)){
      dataRef <- as.Date(paste(year(now()),"-",month(now()),"-01",sep=""))
    }else{
        if (is.null(dataFinal)){
            dataFinal <- as.Date(paste(year(dataRef),"-",month(dataRef)+1,"-01",sep=""))
        }
        #dataRef <- as.Date(paste(year(dataRef),"-",month(dataRef),"-01",sep=""))
    }

    reuniao <- contatosD[(!is.na(contatosD$reuniao_date) &
                              (contatosD$reuniao_date>=dataRef) &
                              (contatosD$reuniao_date<=dataFinal) &
                              contatosD$hubspot_owner_name%in%vendedor &
                              contatosD$lead_source_geral_new%in%lead_source_geral &
                              contatosD$lead_source_new%in%lead_source &
                              contatosD$ecommerce_platform%in%ecommerce_platform),]

    pcontato <- contatosD[(!is.na(contatosD$prim_contato_date) &
                               (contatosD$prim_contato_date>=dataRef) &
                               (contatosD$prim_contato_date<=dataFinal) &
                               contatosD$hubspot_owner_name%in%vendedor &
                               contatosD$lead_source_geral_new%in%lead_source_geral &
                               contatosD$lead_source_new%in%lead_source &
                               contatosD$ecommerce_platform%in%ecommerce_platform),]

    qualificado <- contatosD[(!is.na(contatosD$qualificado_date) &
                                  (contatosD$qualificado_date>=dataRef) &
                                  (contatosD$qualificado_date<=dataFinal) &
                                  contatosD$hubspot_owner_name%in%vendedor &
                                  contatosD$lead_source_geral_new%in%lead_source_geral &
                                  contatosD$lead_source_new%in%lead_source &
                                  contatosD$ecommerce_platform%in%ecommerce_platform),]

    penviada <- contatosD[(!is.na(contatosD$prop_enviada_date) &
                               (contatosD$prop_enviada_date>=dataRef) &
                               (contatosD$prop_enviada_date<=dataFinal) &
                               contatosD$hubspot_owner_name%in%vendedor &
                               contatosD$lead_source_geral_new%in%lead_source_geral &
                               contatosD$lead_source_new%in%lead_source &
                               contatosD$ecommerce_platform%in%ecommerce_platform),]

    paceita <- contatosD[(!is.na(contatosD$prop_aceita_date) &
                              (contatosD$prop_aceita_date>=dataRef) &
                              (contatosD$prop_aceita_date<=dataFinal) &
                              contatosD$hubspot_owner_name%in%vendedor &
                              contatosD$lead_source_geral_new%in%lead_source_geral &
                              contatosD$lead_source_new%in%lead_source &
                              contatosD$ecommerce_platform%in%ecommerce_platform),]

    emintegracao <- contatosD[(!is.na(contatosD$em_integracao_date) &
                                   (contatosD$em_integracao_date>=dataRef) &
                                   (contatosD$em_integracao_date<=dataFinal) &
                                   contatosD$hubspot_owner_name%in%vendedor &
                                   contatosD$lead_source_geral_new%in%lead_source_geral &
                                   contatosD$lead_source_new%in%lead_source &
                                   contatosD$ecommerce_platform%in%ecommerce_platform),]

    integrado <- contatosD[(!is.na(contatosD$integrado_date) &
                                (contatosD$integrado_date>=dataRef) &
                                (contatosD$integrado_date<=dataFinal) &
                                contatosD$hubspot_owner_name%in%vendedor &
                                contatosD$lead_source_geral_new%in%lead_source_geral &
                                contatosD$lead_source_new%in%lead_source &
                                contatosD$ecommerce_platform%in%ecommerce_platform),]

    convertido <- contatosD[(!is.na(contatosD$convertido_date) &
                                 (contatosD$convertido_date>=dataRef) &
                                 (contatosD$convertido_date<=dataFinal) &
                                 contatosD$hubspot_owner_name%in%vendedor &
                                 contatosD$lead_source_geral_new%in%lead_source_geral &
                                 contatosD$lead_source_new%in%lead_source &
                                 contatosD$ecommerce_platform%in%ecommerce_platform &
                                 contatos$deal_stage=="Convertido"),]
  }
  ## visao de trimestral ##
  else if (type == "tri"){

    if (is.null(dataRef)){
      if (is.null(Q)){
        Q <- quarter(now())
      }
      if (Q == 1){
        dataRef <- as.Date(paste(year(now()),"-01-01",sep=""))
      }else if (Q == 2){
        dataRef <- as.Date(paste(year(now()),"-04-01",sep=""))
      }else if (Q == 3){
        dataRef <- as.Date(paste(year(now()),"-07-01",sep=""))
      }else {
        dataRef <- as.Date(paste(year(now()),"-10-01",sep=""))
      }
    }
    else if (!is.null(dataRef)){
      if (is.null(Q)){
        Q <- quarter(dataRef)
      }
      if (Q == 1){
        dataRef <- as.Date(paste(year(dataRef),"-01-01",sep=""))
      }else if (Q == 2){
        dataRef <- as.Date(paste(year(dataRef),"-04-01",sep=""))
      }else if (Q == 3){
        dataRef <- as.Date(paste(year(dataRef),"-07-01",sep=""))
      }else {
        dataRef <- as.Date(paste(year(dataRef),"-10-01",sep=""))
      }
    }

    dataFinal <- dataRef
    month(dataFinal) <- month(dataRef) + 2
    day(dataFinal) <- days_in_month(dataFinal)

    reuniao <- contatosD[(!is.na(contatosD$reuniao_date) &
                              (contatosD$reuniao_date>=dataRef) &
                              (contatosD$reuniao_date<=dataFinal) &
                              contatosD$hubspot_owner_name%in%vendedor &
                              contatosD$lead_source_geral_new%in%lead_source_geral &
                              contatosD$lead_source_new%in%lead_source &
                              contatosD$ecommerce_platform%in%ecommerce_platform),]

    pcontato <- contatosD[(!is.na(contatosD$prim_contato_date) &
                               (contatosD$prim_contato_date>=dataRef) &
                               (contatosD$prim_contato_date<=dataFinal) &
                               contatosD$hubspot_owner_name%in%vendedor &
                               contatosD$lead_source_geral_new%in%lead_source_geral &
                               contatosD$lead_source_new%in%lead_source &
                               contatosD$ecommerce_platform%in%ecommerce_platform),]

    qualificado <- contatosD[(!is.na(contatosD$qualificado_date) &
                                  (contatosD$qualificado_date>=dataRef) &
                                  (contatosD$qualificado_date<=dataFinal) &
                                  contatosD$hubspot_owner_name%in%vendedor &
                                  contatosD$lead_source_geral_new%in%lead_source_geral &
                                  contatosD$lead_source_new%in%lead_source &
                                  contatosD$ecommerce_platform%in%ecommerce_platform),]

    penviada <- contatosD[(!is.na(contatosD$prop_enviada_date) &
                               (contatosD$prop_enviada_date>=dataRef) &
                               (contatosD$prop_enviada_date<=dataFinal) &
                               contatosD$hubspot_owner_name%in%vendedor &
                               contatosD$lead_source_geral_new%in%lead_source_geral &
                               contatosD$lead_source_new%in%lead_source &
                               contatosD$ecommerce_platform%in%ecommerce_platform),]

    paceita <- contatosD[(!is.na(contatosD$prop_aceita_date) &
                              (contatosD$prop_aceita_date>=dataRef) &
                              (contatosD$prop_aceita_date<=dataFinal) &
                              contatosD$hubspot_owner_name%in%vendedor &
                              contatosD$lead_source_geral_new%in%lead_source_geral &
                              contatosD$lead_source_new%in%lead_source &
                              contatosD$ecommerce_platform%in%ecommerce_platform),]

    emintegracao <- contatosD[(!is.na(contatosD$em_integracao_date) &
                                   (contatosD$em_integracao_date>=dataRef) &
                                   (contatosD$em_integracao_date<=dataFinal) &
                                   contatosD$hubspot_owner_name%in%vendedor &
                                   contatosD$lead_source_geral_new%in%lead_source_geral &
                                   contatosD$lead_source_new%in%lead_source &
                                   contatosD$ecommerce_platform%in%ecommerce_platform),]

    integrado <- contatosD[(!is.na(contatosD$integrado_date) &
                                (contatosD$integrado_date>=dataRef) &
                                (contatosD$integrado_date<=dataFinal) &
                                contatosD$hubspot_owner_name%in%vendedor &
                                contatosD$lead_source_geral_new%in%lead_source_geral &
                                contatosD$lead_source_new%in%lead_source &
                                contatosD$ecommerce_platform%in%ecommerce_platform),]

    convertido <- contatosD[(!is.na(contatosD$convertido_date) &
                                 (contatosD$convertido_date>=dataRef) &
                                 (contatosD$convertido_date<=dataFinal) &
                                 contatosD$hubspot_owner_name%in%vendedor &
                                 contatosD$lead_source_geral_new%in%lead_source_geral &
                                 contatosD$lead_source_new%in%lead_source &
                                 contatosD$ecommerce_platform%in%ecommerce_platform &
                                 contatos$deal_stage=="Convertido"),]

  }
  ## visao de safra ##
  else {
    if (is.null(dataRef)){
      dataRef <- as.Date(paste(year(now()),"-01-01",sep=""))
    }
    if (is.null(dataFinal)){
      dataFinal <- as.Date(paste(year(dataRef),"-12-31",sep=""))
    }

    reuniao <- contatosD[(!is.na(contatosD$reuniao_date) &
                              contatosD$reuniao_date>=dataRef &
                            contatosD$reuniao_date<=dataFinal &
                              contatosD$hubspot_owner_name%in%vendedor &
                              contatosD$lead_source_geral_new%in%lead_source_geral &
                              contatosD$lead_source_new%in%lead_source &
                              contatosD$ecommerce_platform%in%ecommerce_platform),]

    pcontato <- contatosD[(!is.na(contatosD$reuniao_date) &
                               contatosD$reuniao_date>=dataRef &
                             contatosD$reuniao_date<=dataFinal &
                               !is.na(contatosD$prim_contato_date) &
                             contatosD$prim_contato_date>=dataRef &
                             contatosD$prim_contato_date<=dataFinal &
                               contatosD$hubspot_owner_name%in%vendedor &
                               contatosD$lead_source_geral_new%in%lead_source_geral &
                               contatosD$lead_source_new%in%lead_source &
                               contatosD$ecommerce_platform%in%ecommerce_platform),]

    qualificado <- contatosD[(!is.na(contatosD$reuniao_date) &
                                  contatosD$reuniao_date>=dataRef &
                                contatosD$reuniao_date<=dataFinal &
                                  !is.na(contatosD$qualificado_date) &
                                contatosD$qualificado_date>=dataRef &
                                contatosD$qualificado_date<=dataFinal &
                                  contatosD$hubspot_owner_name%in%vendedor &
                                  contatosD$lead_source_geral_new%in%lead_source_geral &
                                  contatosD$lead_source_new%in%lead_source &
                                  contatosD$ecommerce_platform%in%ecommerce_platform),]

    penviada <- contatosD[(!is.na(contatosD$reuniao_date) &
                               contatosD$reuniao_date>=dataRef &
                             contatosD$reuniao_date<=dataFinal &
                               !is.na(contatosD$prop_enviada_date) &
                             contatosD$prop_enviada_date>=dataRef &
                             contatosD$prop_enviada_date<=dataFinal &
                               contatosD$hubspot_owner_name%in%vendedor &
                               contatosD$lead_source_geral_new%in%lead_source_geral &
                               contatosD$lead_source_new%in%lead_source &
                               contatosD$ecommerce_platform%in%ecommerce_platform),]

    paceita <- contatosD[(!is.na(contatosD$reuniao_date) &
                              contatosD$reuniao_date>=dataRef &
                            contatosD$reuniao_date<=dataFinal &
                              !is.na(contatosD$prop_aceita_date) &
                            contatosD$prop_aceita_date>=dataRef &
                            contatosD$prop_aceita_date<=dataFinal &
                              contatosD$hubspot_owner_name%in%vendedor &
                              contatosD$lead_source_geral_new%in%lead_source_geral &
                              contatosD$lead_source_new%in%lead_source &
                              contatosD$ecommerce_platform%in%ecommerce_platform),]

    emintegracao <- contatosD[(!is.na(contatosD$reuniao_date) &
                                   contatosD$reuniao_date>=dataRef &
                                 contatosD$reuniao_date<=dataFinal &
                                   !is.na(contatosD$em_integracao_date) &
                                 contatosD$em_integracao_date>=dataRef &
                                 contatosD$em_integracao_date<=dataFinal &
                                   contatosD$hubspot_owner_name%in%vendedor &
                                   contatosD$lead_source_geral_new%in%lead_source_geral &
                                   contatosD$lead_source_new%in%lead_source &
                                   contatosD$ecommerce_platform%in%ecommerce_platform),]

    integrado <- contatosD[(!is.na(contatosD$reuniao_date) &
                                contatosD$reuniao_date>=dataRef &
                              contatosD$reuniao_date<=dataFinal &
                                !is.na(contatosD$integrado_date) &
                              contatosD$integrado_date>=dataRef &
                              contatosD$integrado_date<=dataFinal &
                                contatosD$hubspot_owner_name%in%vendedor &
                                contatosD$lead_source_geral_new%in%lead_source_geral &
                                contatosD$lead_source_new%in%lead_source &
                                contatosD$ecommerce_platform%in%ecommerce_platform),]

    convertido <- contatosD[(!is.na(contatosD$reuniao_date) &
                                 contatosD$reuniao_date>=dataRef &
                               contatosD$reuniao_date<=dataFinal &
                                 !is.na(contatosD$convertido_date) &
                               contatosD$convertido_date>=dataRef &
                               contatosD$convertido_date<=dataFinal &
                                 contatosD$hubspot_owner_name%in%vendedor &
                                 contatosD$lead_source_geral_new%in%lead_source_geral &
                                 contatosD$lead_source_new%in%lead_source &
                                 contatosD$ecommerce_platform%in%ecommerce_platform &
                                 contatos$deal_stage=="Convertido"),]
  }

  reuniao$estagio <- "Reuniao"
  pcontato$estagio <- "Primeiro Contato"
  qualificado$estagio <- "Qualificado"
  penviada$estagio <- "Proposta Enviada"
  paceita$estagio <- "Proposta Aceita"
  emintegracao$estagio <- "Em Integracao"
  integrado$estagio <- "Integrado"
  convertido$estagio <- "Convertido"
  final <- rbind(reuniao,pcontato,qualificado,penviada,paceita,emintegracao,integrado,convertido)

  return (final)
}
#format contacts csv
formatContactcsv <- function(contacts) {
  contacts$opp_new_date<-as.Date(contacts$opp_new_date)
  contacts$became_customer_new<- as.Date(contacts$became_customer_new)
  contacts$reuniao_date<- as.Date(contacts$reuniao_date)
  contacts$prim_contato_date<-as.Date(contacts$prim_contato_date)
  contacts$qualificado_date<-as.Date(contacts$qualificado_date)
  contacts$prop_enviada_date<-as.Date(contacts$prop_enviada_date)
  contacts$prop_aceita_date<- as.Date(contacts$prop_aceita_date)
  contacts$em_integracao_date<-as.Date(contacts$em_integracao_date)
  contacts$integrado_date<-as.Date(contacts$integrado_date)
  contacts$convertido_date<-as.Date(contacts$convertido_date)
  contacts$nao_e_pra_agora_date<-as.Date(contacts$nao_e_pra_agora_date)
  contacts$perdido_date<-as.Date(contacts$perdido_date)
  contacts$hs_lifecyclestage_lead_date<-as.Date(contacts$hs_lifecyclestage_lead_date)
  contacts$hs_lifecyclestage_marketingqualifiedlead_date<-as.Date(contacts$hs_lifecyclestage_marketingqualifiedlead_date)
  contacts$lastmodifieddate<-as.Date(contacts$lastmodifieddate)
  contacts$account_id<-as.numeric(contacts$account_id)
  contacts$hubspotscore<-as.numeric(contacts$hubspotscore)
  contacts$receita_ano<-as.numeric(contacts$receita_ano)
  contacts$receita_mes<-as.numeric(contacts$receita_mes)
  contacts$receita_total<-as.numeric(contacts$receita_total)
  contacts$total_tpv<-as.numeric(contacts$total_tpv)
  contacts$tpv_ano<-as.numeric(contacts$tpv_ano)
  contacts$tpv_anteontem<-as.numeric(contacts$tpv_anteontem)
  contacts$tpv_ontem<-as.numeric(contacts$tpv_ontem)
  contacts$tpv_max<-as.numeric(contacts$tpv_max)
  contacts$tpv_mes<-as.numeric(contacts$tpv_mes)
  contacts$tpv_mes_1<-as.numeric(contacts$tpv_mes_1)
  contacts$tpv_mes_2<-as.numeric(contacts$tpv_mes_2)
  contacts$tpv_mes_3<-as.numeric(contacts$tpv_mes_3)
}
#gera resumo de cohort
cohortSummary <- function (dataRaw = NULL, tpv_group = "all", path = NULL) {
  start_time <- Sys.time()
  print("Working on it...")

  if (is.null(dataRaw)){
    if (is.null(path)) {
      path <- readline(prompt="Enter complete file path: ")
    }
    if(is.null(dataRaw$owner_name)){
      dataRaw$owner_name <- NA
    }
    dataRaw<- read.csv(path,stringsAsFactors = FALSE,encoding = "UTF-8")

    dataRaw$tpv.group[dataRaw$tpv.group=="Large"]<-"F5"

    names(dataRaw) <- c("account_id","tpv_group_bd","login","payment_date","account_fauth","state","super_integrator", "tpv","receita",
                        "n_payments_submitted", "n_payments_canceled", "n_payments_authorized", "n_payments_analysis", "n_payments_pre",
                        "n_payments")
  }else if (is.null(dataRaw$owner_name)){
    dataRaw$owner_name <- NA
    dataRaw$tpv.group[dataRaw$tpv.group=="Large"]<-"F5"

    names(dataRaw) <- c("account_id","tpv_group_bd","login","payment_date","account_fauth","state","super_integrator", "tpv","receita",
                        "n_payments_submitted", "n_payments_canceled", "n_payments_authorized", "n_payments_analysis", "n_payments_pre",
                        "n_payments", "owner_name")
  }else {
    dataRaw$tpv.group[dataRaw$tpv.group=="Large"]<-"F5"

    names(dataRaw) <- c("account_id","tpv_group_bd","login","payment_date","account_fauth","state","super_integrator", "tpv","receita",
                        "n_payments_submitted", "n_payments_canceled", "n_payments_authorized", "n_payments_analysis", "n_payments_pre",
                        "n_payments", "owner_name")
  }


  grp_data <- group_by(dataRaw, account_id, login, tpv_group_bd, account_fauth, payment_date)
  sum_data <- summarise(grp_data, tpv = sum(tpv), receita = sum(receita), n_payments_submitted = sum(n_payments_submitted),
                        n_payments_canceled = sum(n_payments_canceled), n_payments_authorized = sum(n_payments_authorized),
                        n_payments_analysis = sum(n_payments_analysis), n_payments_pre = sum(n_payments_pre),
                        n_payments = sum(n_payments))

  fatData <- sum_data[!(sum_data$payment_date==""),]
  grp_fat <- group_by(fatData, account_id)
  sum_fat <- summarise(grp_fat, tpv = max(tpv, na.rm = TRUE))

  sum_fat$tpv_group<-NA
  sum_fat$tpv_group[sum_fat$tpv >=0 & sum_fat$tpv <1000] <- "F0"
  sum_fat$tpv_group[sum_fat$tpv >=1000 & sum_fat$tpv <5000] <- "F1"
  sum_fat$tpv_group[sum_fat$tpv >=5000 & sum_fat$tpv <20000] <- "F2"
  sum_fat$tpv_group[sum_fat$tpv >=20000 & sum_fat$tpv <100000] <- "F3"
  sum_fat$tpv_group[sum_fat$tpv >=100000 & sum_fat$tpv <500000] <- "F4"
  sum_fat$tpv_group[sum_fat$tpv >=500000 & sum_fat$tpv <1000000] <- "F5"
  sum_fat$tpv_group[sum_fat$tpv >=1000000 & sum_fat$tpv <5000000] <- "F6"
  sum_fat$tpv_group[sum_fat$tpv >=5000000 & sum_fat$tpv] <- "F7"

  sum_fat <- sum_fat[,c(which(names(sum_fat)=="account_id"),which(names(sum_fat)=="tpv_group"))]

  sum_data <- merge(sum_data, sum_fat, by = "account_id", all = TRUE)

  if (tpv_group == "all") {
    tpv_group <- unique(sum_data$tpv_group)
  }

  data <- sum_data[sum_data$tpv_group%in%tpv_group&!(sum_data$payment_date==""),]
  #data <- dataRaw[dataRaw$tpv.group%in%tpv_group,]



  data$payment_date <- as.yearmon(data$payment_date,"%m/%Y")
  data$account_fauth <- as.Date(data$account_fauth)
  data$account_fauth <- as.yearmon(data$account_fauth,"%m/%Y")

  ##arrange date first payment
  data <- data[order(data$account_id),]
  grp <- group_by(data,account_id)
  fdTPV <- summarise(grp, first_payment = min(payment_date))
  ldTPV <- summarise(grp, last_payment = max(payment_date))
  data <- Reduce(function(x, y) merge(x, y, by = "account_id", all=TRUE), list(data,fdTPV,ldTPV))

  data$m <- ((year(data$payment_date) - year(data$first_payment))*12 + (month(data$payment_date) - month(data$first_payment)))

  #organize mX
  all_m <- 0:max(data$m)
  diff_m <- setdiff(all_m,unique(data$m))

  if (length(diff_m)>0){
    for (i in 1:length(diff_m)){
      data[dim(data)[1]+1,] <- NA
      data[dim(data)[1],which(names(data)=="m")] <- diff_m[i]
    }
  }

  #organize first_payment
  max_date <- max(unique(data$payment_date),na.rm = TRUE)
  min_date <- min(unique(data$first_payment),na.rm = TRUE)
  all_date <- as.yearmon(seq(as.numeric(min_date),as.numeric(max_date),by = 1/12))
  diff_date <- as.yearmon(setdiff(all_date,unique(data$first_payment)))

  if (length(diff_date)>0){
    for (i in 1:length(diff_date)){
      data[dim(data)[1]+1,] <- NA
      data[dim(data)[1],which(names(data)=="first_payment")] <- diff_date[i]
    }
  }

  ##client count
  client_table <- xtabs(~first_payment+m,data = data, na.action = na.pass)
  client_count <- colSums(client_table)
  client_count_perc <- round((client_count/client_count[1]),digits = 4)

  ##tpv count/perc
  tpv_table <- xtabs(tpv~first_payment+m,data = data, na.action = na.pass)
  client_tpv_sum <- colSums(tpv_table)
  client_tpv <- colSums(tpv_table)
  client_tpv[1] <- client_tpv[1]/client_count[1]
  for (i in 2:length(client_tpv)){

    client_tpv[i] <- (client_tpv[i]/client_count[1]) + client_tpv[i-1]
  }
  client_tpv_perc <- round((client_tpv/client_tpv[1]),digits = 4)

  client_tpv_perc <- client_tpv_sum
  for (i in 1:(length(client_tpv_perc)-1)){
    if(client_tpv_sum[i]!=0){
      client_tpv_perc[i] <- (client_tpv_sum[i])/sum(tpv_table[(1:(length(tpv_table[,1])-i+1)),1])
    }
    else {
      client_tpv_perc[i] <- 1
    }
  }

  ##receita count/perc
  receita_table <- xtabs(receita~first_payment+m,data = data, na.action = na.pass)
  client_receita_sum <- colSums(receita_table)
  client_receita <- colSums(receita_table)
  client_receita[1] <- client_receita[1]/client_count[1]
  for (i in 2:length(client_receita)){

    client_receita[i] <- (client_receita[i]/client_count[1]) + client_receita[i-1]
  }

  client_receita_perc <- client_receita_sum
  for (i in 1:length(client_receita_perc)){
    if(client_receita_sum[i]!=0){
      client_receita_perc[i] <- (client_receita_sum[i])/sum(receita_table[(1:(length(receita_table[,1])-i+1)),1])
    }
    else {
      client_receita_perc[i] <- 1
    }
  }


  ##churn mes
  churn_perc <- client_count
  for (i in 1:(length(churn_perc)-1)){
    if(client_count[i]!=0){
      churn_perc[i] <- ((client_count[i] - client_count[i+1] - client_table[(length(client_table[,1])-i+1),i])/client_count[i])
    }
    else {
      churn_perc[i] <- 1
    }
  }


  churn_perc[-length(churn_perc)]

  ##summary data
  summary <- list (data = data,
                   client_table = client_table, client_count = client_count,client_count_perc = client_count_perc,
                   tpv_table = tpv_table, client_tpv = client_tpv, client_tpv_perc = client_tpv_perc,
                   receita_table = receita_table, client_receita = client_receita, client_receita_perc = client_receita_perc,
                   churn_perc = churn_perc)

  print(Sys.time()-start_time)

  return(summary)
}
#gera resumo de cohort para plot
churnBase <- function(base , comercial, churn_time = 4, tpv_group = c("F0","F1","F2","F3","F4","F5", "F6", "F7"), type = "all") {

  #comercial<- read.csv("C:/Users/mauricio.chigutti/Google Drive/Clientes do Comercial/clientesComercial.csv",
  #                     stringsAsFactors = FALSE, sep = ";")
  comercial <- comercial[comercial$Account.Id!=0,c(2,3)]
  names(comercial) <- c("owner_name","account.id")

  base <- merge(base,comercial,by = "account.id", all = TRUE)

  if ( type == "touch"){
    base <- base[!is.na(base$owner_name),]
  }else if(type == "touchless"){
    base <- base[is.na(base$owner_name),]
  }

  table <- cohortSummary(base,tpv_group = tpv_group)

  month_now <- as.yearmon(now())
  month_min <- month_now - (churn_time-1)/12

  n <- 1
  mymonths <- vector()
  mymonths[1] <- month_now
  while(churn_time-n > 0){
    mymonths[n+1] <- month_now - (n)/12
    n = n + 1
  }
  mymonths <- as.character(as.yearmon(mymonths))

  final <- list()

  final$client_list <- table$data[table$data$first_payment >= month_min,]
  final$client_list <- final$client_list[!is.na(final$client_list$account_id),]

  final$table <- table$client_table[rownames(table$client_table)%in%mymonths,c(1:churn_time)]

  ##churn mes
  client_perc <- colSums(final$table)
  client_sum <- colSums(final$table)
  for (i in 1:(length(client_perc))){
    if(client_sum[i]!=0){
      client_perc[i] <- (client_sum[i])/sum(final$table[(1:(length(final$table[,1])-i+1)),1])
    }
    else {
      client_perc[i] <- 1
    }
  }

  final$client_perc <- 1-client_perc

  return (final)
}
#le base do goodata para cohort
getCohortBase <- function(path = "C:/Users/mauricio.chigutti/Google Drive/Cohort/Churn4.csv"){
  base<- read.csv(path,
                  stringsAsFactors = FALSE,encoding = "UTF-8")

  return(base)
}
#gera lista com churns do MX a partir de uma lista de contatos
filtraCohort <- function(lista_clientes, churn_time, churn_month) {

  month_now <- as.yearmon(now())
  month_min <- month_now - (churn_time-1)/12
  month_max <- month_now - (churn_month)/12

  if(churn_month!=0){

    #lista_clientes$m_novo <- paste0("M",lista_clientes$m)
    lista_clientes$m_novo <- lista_clientes$m
    final <- dcast(data = lista_clientes, account_id + login + first_payment + last_payment + tpv_group  ~ m_novo, value.var = "tpv")
    final <- final[final$first_payment <= month_max,]
    final <- final[is.na(final[,which(names(final)==churn_month)]),]
  }else {

    #lista_clientes$m_novo <- paste0("M",lista_clientes$m)
    lista_clientes$m_novo <- lista_clientes$m
    final <- dcast(data = lista_clientes, account_id + login + first_payment + last_payment + tpv_group  ~ m_novo, value.var = "tpv")
    final <- final[final$last_payment < month_now,]
  }

  return (final)
}
#gera tabela de dias médios para cada vendedor
funilTempoMedio <- function(contatos, vendedor = "all", lead_source_geral = "all", lead_source = "all", ecommerce_platform = "all"){

  ## set vectors for all option ##
  if (vendedor == "all"){
    vendedor <- unique(contatos$hubspot_owner_name)[!is.na(unique(contatos$hubspot_owner_name))]
  }
  if (lead_source_geral == "all"){
    lead_source_geral <- unique(contatos$lead_source_geral_new)
  }
  if (lead_source == "all"){
    lead_source <- unique(contatos$lead_source_new)
  }
  if (ecommerce_platform == "all"){
    ecommerce_platform <- unique(contatos$ecommerce_platform)
  }

  unicos <- aggregate(contatos, by=list(contatos$email), FUN = min)
  unicos$RPC <- as.Date(unicos$prim_contato_date)-as.Date(unicos$reuniao_date)
  unicos$PCQ <- as.Date(unicos$qualificado_date)-as.Date(unicos$prim_contato_date)
  unicos$QPE <- as.Date(unicos$prop_enviada_date)-as.Date(unicos$qualificado_date)
  unicos$PEPA <- as.Date(unicos$prop_aceita_date)-as.Date(unicos$prop_enviada_date)
  unicos$PAEI <- as.Date(unicos$em_integracao_date)-as.Date(unicos$prop_aceita_date)
  unicos$EII <- as.Date(unicos$integrado_date)-as.Date(unicos$em_integracao_date)
  unicos$IC <- as.Date(unicos$convertido_date)-as.Date(unicos$integrado_date)
  unicos$AteConverter <- as.Date(unicos$convertido_date)-as.Date(unicos$reuniao_date)

  final_unicos <- unicos[unicos$hubspot_owner_name%in% vendedor & unicos$lead_source_geral_new%in%lead_source_geral &
                    unicos$lead_source_new%in%lead_source & unicos$ecommerce_platform%in%ecommerce_platform,]

  group <- group_by(final_unicos, hubspot_owner_name)
  sm <- summarise(group, AteConverter = round(mean(AteConverter, na.rm = TRUE), digits=0), RPC = round(mean(RPC, na.rm = TRUE), digits=0),
                  PCQ = round(mean(PCQ, na.rm = TRUE), digits=0), QPE = round(mean(QPE, na.rm = TRUE), digits=0),
                  PEPA = round(mean(PEPA, na.rm = TRUE), digits=0), PAEI = round(mean(PAEI, na.rm = TRUE), digits=0),
                  EII = round(mean(EII, na.rm = TRUE), digits=0), IC = round(mean(IC, na.rm = TRUE), digits=0))

  resumo <- list()
  for(i in 1:length(vendedor)){

    AteConverter <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$AteConverter),]$AteConverter))
    AteConverter <- data.frame(mean = round(mean(AteConverter, na.rm = TRUE), digits=1), sd = round(sd(AteConverter, na.rm = TRUE), digits=1), max = round(max(AteConverter, na.rm = TRUE), digits=1), min = round(min(AteConverter, na.rm = TRUE), digits=1), median = round(median(AteConverter, na.rm = TRUE), digits=1))

    RPC <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$RPC),]$RPC))
    RPC <- data.frame(mean = round(mean(RPC, na.rm = TRUE), digits=1), sd = round(sd(RPC, na.rm = TRUE), digits=1), max = round(max(RPC, na.rm = TRUE), digits=1), min = round(min(RPC, na.rm = TRUE), digits=1), median = round(median(RPC, na.rm = TRUE), digits=1))

    PCQ <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$PCQ),]$PCQ))
    PCQ <- data.frame(mean = round(mean(PCQ, na.rm = TRUE), digits=1), sd = round(sd(PCQ, na.rm = TRUE), digits=1), max = round(max(PCQ, na.rm = TRUE), digits=1), min = round(min(PCQ, na.rm = TRUE), digits=1), median = round(median(PCQ, na.rm = TRUE), digits=1))

    QPE <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$QPE),]$QPE))
    QPE <- data.frame(mean = round(mean(QPE, na.rm = TRUE), digits=1), sd = round(sd(QPE, na.rm = TRUE), digits=1), max = round(max(QPE, na.rm = TRUE), digits=1), min = round(min(QPE, na.rm = TRUE), digits=1), median = round(median(QPE, na.rm = TRUE), digits=1))

    PEPA <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$PEPA),]$PEPA))
    PEPA <- data.frame(mean = round(mean(PEPA, na.rm = TRUE), digits=1), sd = round(sd(PEPA, na.rm = TRUE), digits=1), max = round(max(PEPA, na.rm = TRUE), digits=1), min = round(min(PEPA, na.rm = TRUE), digits=1), median = round(median(PEPA, na.rm = TRUE), digits=1))

    PAEI <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$PAEI),]$PAEI))
    PAEI <- data.frame(mean = round(mean(PAEI, na.rm = TRUE), digits=1), sd = round(sd(PAEI, na.rm = TRUE), digits=1), max = round(max(PAEI, na.rm = TRUE), digits=1), min = round(min(PAEI, na.rm = TRUE), digits=1), median = round(median(PAEI, na.rm = TRUE), digits=1))

    EII <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$EII),]$EII))
    EII <- data.frame(mean = round(mean(EII, na.rm = TRUE), digits=1), sd = round(sd(EII, na.rm = TRUE), digits=1), max = round(max(EII, na.rm = TRUE), digits=1), min = round(min(EII, na.rm = TRUE), digits=1), median = round(median(EII, na.rm = TRUE), digits=1))

    IC <- (as.numeric(unicos[unicos$hubspot_owner_name==vendedor[i] & !is.na(unicos$hubspot_owner_name) & !is.na(unicos$IC),]$IC))
    IC <- data.frame(mean = round(mean(IC, na.rm = TRUE), digits=1), sd = round(sd(IC, na.rm = TRUE), digits=1), max = round(max(IC, na.rm = TRUE), digits=1), min = round(min(IC, na.rm = TRUE), digits=1), median = round(median(IC, na.rm = TRUE), digits=1))

    resumo[[vendedor[i]]] <- rbind(AteConverter,RPC,PCQ,QPE,PEPA,PAEI,EII,IC)
    rownames(resumo[[vendedor[i]]]) <- c("AteConverter","RPC","PCQ","QPE","PEPA","PAEI","EII","IC")
  }

  final <- list(table_sm = sm, resumo = resumo)
}
#cria word cloud a partir de texto
topWords <- function(texto ,n_words = 1, remove_words = TRUE, remove_punctuation = TRUE,
                     to_lower = TRUE, strip_space = TRUE){
  docs <- VCorpus(VectorSource(texto))
  #processing
  if(remove_punctuation){
    docs <- tm_map(docs,removePunctuation)
  }
  if(to_lower){
    docs <- tm_map(docs, tolower)
  }
  if(remove_words){
    docs <- tm_map(docs, removeWords, stopwords("portuguese"))
  }
  if(strip_space){
    docs <- tm_map(docs, stripWhitespace)
  }

  docs <- tm_map(docs, PlainTextDocument)

  BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), n_words), paste, collapse = " "), use.names = FALSE)
  }


  tdm <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))

  twogram_freq <- sort(row_sums(tdm), decreasing=TRUE)
  twogram_freq_df <- data.frame(words = names(twogram_freq), word_freq = twogram_freq)
  soma <- sum(twogram_freq_df$word_freq)
  twogram_freq_df$density <- format(twogram_freq_df$word_freq/soma,digits=2)
  return(twogram_freq_df)
}
#plot de word cloud
plotWcloud <- function(wordsDf, wmax = 50){
  wordcloud((wordsDf$words), wordsDf$word_freq, max.words=wmax, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
}
