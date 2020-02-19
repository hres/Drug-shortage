library(jsonlite)
library(lubridate)
library(dplyr)
library(plotly)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(purrr)
library(elastic)



#function to assemble query:
add_quote <- function(string) {
  string <- gsub("'", "", string)
  #string <- gsub("&", "\&", string)
 string<-paste0('\\\"',string,'\\\" ')
 string
  
}

create_query<-function(ing,company=NULL){
  search_uri<-paste0('en_ingredients:',add_quote(ing))
  
  if(!is.null(company))
    search_uri<-paste0(search_uri,' AND drug.company.name:',add_quote(company))
  
  return(search_uri)
}


result<-function(search_uri){
  
query<-paste0('{"query":{"query_string":{"query":"',search_uri,'"}}}')
r <-Search(index='drug_shortages_canada',body=query,raw=T)%>%fromJSON()
r<-r$hits$hits

r<-r$`_source`

date_col<-c('actual_start_date','actual_end_date')
r[,date_col]<-lapply(r[,date_col],function(x){x<-ymd(substr(x,1,10))})

r<-r%>%select(actual_start_date,actual_end_date,en_drug_brand_name,en_ingredients,en_drug_common_name,
                company_name,atc_number,din,shortage_reason)
r<-cbind(r[,1:7],r$shortage_reason[,2])
r<-r%>%filter(actual_start_date<actual_end_date)

return(r)
}


ing<-'
{
  "aggs" : {
  "ing" : {
  "terms" : {
  "field" : "en_ingredients.keyword",
  "size":10000
  }
  }
  }
  }'

active_ing<-Search(index='drug_shortages_canada',body=ing,size=0)$aggregations$ing$buckets
active_ing<-lapply(active_ing,'[','key')%>%unlist()%>%unique()
