source('run.R')
source('google.R')
library(rjson)
library(XML)
library(utils)
library(RCurl)
library(plyr)

snooze <- function(sleepSec) if (sleepSec>0) Sys.sleep(sleepSec)

findRoutes <- function(loc="Nashville, TN",sleep=1,maxRoutes=NA){
  runRoot <- 'http://runkeeper.com'
  runURL <- 'http://runkeeper.com/search/routes'

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
      maxRoutes <<- maxRoutes - length(links)
      return(ifelse(maxRoutes <= 0,TRUE,FALSE))
    } else {
      return(FALSE)
    }
  }

  build_answer <- function(){
    data.frame(
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
    if (length(link_nodes) == 0) return()
    links <- c(links,unlist(lapply(link_nodes,function(i) i['link'])))
    titles <- c(titles,unlist(getNodeSet(x,'//div[@class="resultListItem clearfix"]/div/h4[@class="resultTitle"]',fun=xmlValue)))
    if (foundMaxRoutes()) return(build_answer())
    next_link_node <- getNodeSet(x,'//a[@class="nextLink"]',fun=xmlAttrs)
    if (length(next_link_node) == 0) return()
    next_rel_link <- next_link_node[[1]]['href']
  } 
}

getRoute <- function(link=''){
  if (is.data.frame(link)){
    # TODO: vectorize
    link <- link$link[1]
  }

  x <- htmlParse(getURL(link),asText=TRUE)
  x <- getNodeSet(x,'//div[@class="mainColumnPadding"]/script',fun=xmlValue)[[1]]
  x <- strsplit(x,split="\n")[[1]][2]
  x <- sub('^\\s*var\\s+routePoints\\s+=\\s+','',x)
  x <- sub(';$','',x)

  # Treat all NULL values as empty strings. This will allow us to use unlist 
  # which unfortunately would drops NULL values.
  y <- lapply(fromJSON(x),function(i) lapply(i,function(o) if (is.null(o)) "" else o) )

  if (length(y) <= 0) return(data.frame())

  colNames <- names(y[[1]])
  numRows <- length(y)
  z <- unlist(y)
  dim(z) <- c(length(colNames),numRows)
  zz <- as.data.frame(t(z),stringsAsFactors = FALSE)
  names(zz) <- colNames

  zz
}


