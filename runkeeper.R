source('run.R')
source('google.R')
library(rjson)
library(XML)
library(utils)
library(RCurl)
library(plyr)

snooze <- function(sleepSec) if (sleepSec>0) Sys.sleep(sleepSec)

findRoutes <- function(location="Nashville, TN",sleep=1,maxRoutes=NA){
  runRoot <- 'http://runkeeper.com'
  runURL <- 'http://runkeeper.com/search/routes'

  adply(location,.margins=1,.fun=function(loc){
    latlong <- gGeoCode(loc)
    search_form <-
      list(
        activityType='RUN',
        location=loc,
        lat=latlong[1],
        lon=latlong[2]
      )

    x = htmlParse(postForm(runURL,.params=search_form),asText=TRUE)

    link_nodes <- getNodeSet(x,'//div[@class="resultListItem clearfix"]',fun=xmlAttrs)

    if (length(link_nodes) == 0) return()

    links <- unlist(lapply(link_nodes,function(i) i['link']))

    titles <- unlist(getNodeSet(x,'//div[@class="resultListItem clearfix"]/div/h4[@class="resultTitle"]',fun=xmlValue))


    foundMaxRoutes <- function(){
      if (!is.na(maxRoutes)){
        maxRoutes <<- maxRoutes - length(link_nodes)
        return(ifelse(maxRoutes <= 0,TRUE,FALSE))
      } else {
        return(FALSE)
      }
    }

    build_answer <- function(){
      data.frame(
        location=loc,
        name=titles,
        link=paste(runRoot,links,sep=''),
        stringsAsFactors=FALSE
      )
    }

    if (foundMaxRoutes()) return(build_answer())

    next_link_node <- getNodeSet(x,'//a[@class="nextLink"]',fun=xmlAttrs)

    if (length(next_link_node) == 0) return()

    next_rel_link <- next_link_node[[1]]['href']

    snooze(sleep)

    while (isTRUE(nzchar(next_rel_link))) {
      next_link <- paste(runRoot,next_rel_link,sep='')
      x = htmlParse(getURL(next_link),asText=TRUE)
      link_nodes <- getNodeSet(x,'//div[@class="resultListItem clearfix"]',fun=xmlAttrs)
      if (length(link_nodes) == 0) return(build_answer())
      links <- c(links,unlist(lapply(link_nodes,function(i) i['link'])))
      titles <- c(titles,unlist(getNodeSet(x,'//div[@class="resultListItem clearfix"]/div/h4[@class="resultTitle"]',fun=xmlValue)))
      if (foundMaxRoutes()) return(build_answer())
      next_link_node <- getNodeSet(x,'//a[@class="nextLink"]',fun=xmlAttrs)
      if (length(next_link_node) == 0) return(build_answer())
      next_rel_link <- next_link_node[[1]]['href']
    } 

    build_answer()
  })
}

getRoute <- function(link=''){
  if (!is.data.frame(link)){
    link <- data.frame(location='Unknown',name='Unknown',link=link)
  }

  route <- function(piece,...){
    x <- htmlParse(getURL(piece$link),asText=TRUE)
    x <- getNodeSet(x,'//div[@class="mainColumnPadding"]/script',fun=xmlValue)[[1]]
    payload <- try(strsplit(x,split="\n")[[1]][2])
    if (inherits(payload,'try-error'))
      return(data.frame(lat=x,lon=NULL,stringsAsFactors=FALSE))
    x <- sub('^\\s*var\\s+routePoints\\s+=\\s+','',payload)
    x <- sub(';$','',x)

    ldply(fromJSON(x),function(i)data.frame(lat=i$latitude,lon=i$longitude,stringsAsFactors=FALSE) )
  }

  ddply( link, .(location,name),.fun=route)
}


