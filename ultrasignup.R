source('run.R')
library(ggmap)
library(rjson)
library(XML)
library(utils)
library(RCurl)
library(plyr)

snooze <- function(sleepSec) if (sleepSec>0) Sys.sleep(sleepSec)

ultraCookies <- function(){
  cookieFile <- file.path(tempdir(),'ultrasignup_cookies.txt')
  if (!file.exists(cookieFile))
    invisible(getURL('http://ultrasignup.com',.opts=curlOptions(header=TRUE,cookiejar=cookieFile)))
  cookieFile
}

ultraOpts <- function() curlOptions(cookiejar=ultraCookies())

fromJSONDF <- function(jsonURL){
  con <- url(jsonURL)
  x <- fromJSON(file=con)
  close(con)
  # Treat all NULL values as empty strings. This will allow us to use unlist 
  # which unfortunately would drops NULL values.
  y <- lapply(x,function(i) lapply(i,function(o) if (is.null(o)) "" else o) )

  if (length(y) <= 0) return(data.frame())

  colNames <- names(y[[1]])
  numRows <- length(y)
  z <- unlist(y)
  dim(z) <- c(length(colNames),numRows)
  zz <- as.data.frame(t(z),stringsAsFactors = FALSE)
  names(zz) <- colNames

  zz
}

queryEvent <- function(q=''){
  ultraURL <- sprintf('http://ultrasignup.com/service/events.svc/GetFeaturedEventsSearch/p=0/q=%s',URLencode(q,reserve=TRUE))
  fromJSONDF(ultraURL)
}

queryEventByLoc <- function(q=NULL,lat=NULL,lon=NULL){
  if (is.null(q)){
    g <- list(lat=lat,lon=lon)
  } else {
    g <- geocode(q)
  }
  lat <- URLencode(format(g$lat))
  lng <- URLencode(format(g$lon))
  
  ultraURL <- sprintf('http://ultrasignup.com/service/events.svc/closestevents?lat=%s&lng=%s&mi=300&mo=12',lat,lng)
  fromJSONDF(ultraURL)
}

queryRunner<- function(q=''){
  ultraURL <- sprintf('http://ultrasignup.com/service/events.svc/GetParticipantSearch/p=0/q=%s',URLencode(q,reserve=TRUE))
  fromJSONDF(ultraURL)
}

queryEntrants <- function(race=''){
  if (is.data.frame(race)){
    dframe <- race
  } else if (!nzchar(race)){
    # TODO: vectorize
    dframe <- queryEvent(race)
  }
  eid <- dframe$eventId[1]
  eventDistances<-dframe$eventDistances
  rootURL <- 'http://ultrasignup.com/'
  ultraURL <- sprintf('http://ultrasignup.com/register.aspx?eid=%d',as.integer(eid))

  x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE)

  if (xmlValue(getNodeSet(x,'//title')[[1]])=='Object moved'){
    ultraURL <- paste(rootURL,xmlAttrs(getNodeSet(x,'//a')[[1]]),sep='')
    x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE)
  }

  x <- getNodeSet(x,'//a[@id="ContentPlaceHolder1_EventInfo1_hlEntrants"]')
  if (length(x)>0){
    ultraURL <- paste(rootURL,xmlAttrs(x[[1]])['href'],sep='')
    x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE)
  } else {
    return(data.frame())
  }

  dateNode <- xmlValue(getNodeSet(x,'//span[@id="lblDate"]')[[1]])
  raceDate <- strptime(strsplit(dateNode,'@')[[1]][1],format="%A, %B %d, %Y")
  
  distances <- 
    ldply(
      getNodeSet(x,'//a[@class="event_selected_link"]|//a[@class="event_link"]'),
      function(i)
        data.frame(
          href=xmlAttrs(i)['href'],
          distance=xmlValue(i),
          selected=xmlAttrs(i)['class']=='event_selected_link',
          stringsAsFactors=FALSE)
    )

  if (nrow(distances)==0){
    # only one distance contested
    distances <- data.frame(href=NA,distance=eventDistances,selected=TRUE)
  }

  thisDistance <- xmlChildren(getNodeSet(x,'//table[@class="ultra_grid"]')[[1]])
  
  trim <- function(x){
    gsub('^\\s+|\\s+$|\\xc2+|\\xa0+','',x,perl=TRUE)
  }
  genderage <- function(x){
    strsplit(sub('.*([FM])(\\d\\d?).*$','\\1 \\2',x),' ')[[1]]
  }

  resultsForDistance <- function(y){
    if (length(y)<=1){
      return(data.frame())
    }
    ret <- data.frame()
    for (i in 2:length(y)){
      dat <- as.character(xmlApply(y[[i]],function(x)trim(xmlValue(x))))
      ga <- genderage(dat[5])
      if (dat[1]==''){
          rank<-NA
      } else {
          rank<-as.numeric(sub('\\s+?%','',trim(dat[1])))
      }
      if (dat[2]==''){
          agerank<-NA
      } else {
          agerank<-as.numeric(sub('\\s+?%','',trim(dat[2])))
      }
      if (dat[4]==''){
          formattarget<-NA
          targettime<-NA
          targettime_hour<-NA
      } else {
          formattarget<-dat[4]
          targettime<-as.integer(str2sec(dat[4]))
          targettime_hour<- as.integer(str2sec(dat[4])) / 3600
      }
      ret <- 
        rbind(
          ret,
          data.frame(
            date=raceDate,
            rank=rank,
            agerank=agerank,
            results=as.integer(dat[3]),
            formattarget=formattarget,
            targettime=targettime,
            targettime_hour=targettime_hour,
            gender=ga[1],
            age=ga[2],
            firstname=dat[6],
            lastname=dat[7],
            city=dat[8],
            state=dat[9],
            bib=dat[11],
            stringsAsFactors=FALSE
          )
        )
    }
    ret 
  }

  ddplyFun <- function(i){
    if (i$selected[1]==TRUE)
      return(resultsForDistance(thisDistance))
    ultraURL <- paste(rootURL,trim(i$href[1]),sep='')
    x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE)
    nodes <- getNodeSet(x,'//table[@class="ultra_grid"]')
    if (length(nodes)==0)
      return(data.frame())
    x <- xmlChildren(nodes[[1]])
    resultsForDistance(x)
  }

  ddply( distances, .(distance),ddplyFun)
}

runnerResults <- function(lname='',fname=''){
  if (is.data.frame(lname)){
    dframe <- lname
    # TODO: vectorize
    lname <- as.character(dframe$LastName)
    fname <- as.character(dframe$FirstName)
  }
  lnames <- lname
  fnames <- fname
  ret <- data.frame()
  for (i in 1:length(lnames)){
    lname <- lnames[i]
    fname <- fnames[i]
    ultraURL <- sprintf('http://ultrasignup.com/results_participant.aspx?fname=%s&lname=%s',URLencode(fname),URLencode(lname))
    x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE)
    x <- getNodeSet(x,'//table[@id="presults"]/*')

    trim <- function(x){
      gsub('^\\s+|\\s+$|\\xc2+|\\xa0+','',x,perl=TRUE)
    }
    genderage <- function(x){
      strsplit(sub('.*([FM])(\\d\\d?).*$','\\1 \\2',x),' ')[[1]]
    }
    ga <- NULL
    for (j in 1:length(x)){
      if (is.null(xmlAttrs(x[[j]]))) next
      if (xmlAttrs(x[[j]])[1]=='groupheader'){
        ga <- genderage(xmlValue(x[[j]]))
        gender <- ifelse(ga[1]=='M','Men','Women')
        known_age <- as.integer(ga[2])
      } else if (xmlAttrs(x[[j]])[1] %in% c('altrow rows','row  rows')){

        dat <- xmlToList(x[[j]])
        dat <- dat[names(dat)=='td']

        ranking <- as.numeric(sub('%','',trim(dat[[1]]$text)))
        place <- as.integer(trim(dat[[2]]$text))
        gender_place <- as.integer(trim(dat[[3]]$text))
        age <- as.integer(trim(dat[[4]]$text))
        formattime <- trim(dat[[5]]$text)
        time  <- as.integer(str2sec(formattime))
        time_hour  <- time / 3600
        eventdate <- strptime(trim(dat[[6]]$text),format="%B %d, %Y")
        year <- as.integer(format(eventdate,'%Y'))
        state <- trim(dat[[7]]$text)

        if (is.null(dat[[8]]$a)){
          race <- strsplit(dat[[8]]$span$text,'-')[[1]]
          distance_spec <- trim(race[length(race)]) # can have two or three elements, last is distance
          race <- trim(race[1])
          did <- NA
        } else {
          race <- strsplit(dat[[8]]$a$text,'-')[[1]]
          distance_spec <- trim(race[length(race)]) # can have two or three elements, last is distance
          race <- trim(race[1])
          did <- as.integer(strsplit(dat[[8]]$a$.attrs,'=')[[1]][2])
        }

        ret <- rbind( ret, data.frame(
              lastname=lname,
              firstname=fname,
              gender=gender,
              known_age=known_age,
              ranking=ranking,
              place=place,
              gender_place=gender_place,
              age=age,
              formattime=formattime,
              time=time,
              time_hour=time_hour,
              eventdate=eventdate,
              year=year,
              state=state,
              race=race,
              distance_spec=distance_spec,
              did=did,
              stringsAsFactors=FALSE
            )
          )
      }
    }
  }
  ret
}

eventResults <- function(eid=NA,sleep=1){
  if (is.data.frame(eid)){
    dframe <- eid
    eid <- dframe$eventId[1]
  } else if (is.na(eid)) return(data.frame())
  rootURL <- 'http://ultrasignup.com/'
  ultraURL <- sprintf('http://ultrasignup.com/register.aspx?eid=%d',as.integer(eid))
  # ultraURL <- "http://ultrasignup.com/register.aspx?did=23884"
  # From the registration page, gather all time ID's:
  # for
  x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE); snooze(sleep)

  if (xmlValue(getNodeSet(x,'//title')[[1]])=='Object moved'){
    ultraURL <- paste('http://ultrasignup.com',xmlAttrs(getNodeSet(x,'//a')[[1]]),sep='')
    # Event Id unknown
    if (rootURL == ultraURL) return(data.frame())
    x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE); snooze(sleep)
  }
  # getNodeSet(x,'//table[@id="ContentPlaceHolder1_Results11_dlResultYears"]//a')
  timeId <- ldply(getNodeSet(x,'//table[@id="ContentPlaceHolder1_Results11_dlResultYears"]//a'),function(i) data.frame(url=xmlAttrs(i)[["href"]],year=xmlValue(i)))

  if (nrow(timeId) == 0) return(data.frame())

  eventName <- getNodeSet(x,'//h1[@class="event-title"]',fun=xmlValue)[[1]]

  # Take the first record from the timeId and find all distances contested
  x <- htmlParse(paste(rootURL,as.character(timeId$url[1]),sep=''),isURL=TRUE); snooze(sleep)
  distance <- 
    ldply(
      getNodeSet(x,'//div[@class="unit-1-2 text-right"]//a'),
      function(i) {
        data.frame(
          url=xmlAttrs(i)[["href"]],
          distance=xmlValue(i),
          selected=ifelse(xmlAttrs(i)[["class"]]=='event_selected_link',TRUE,FALSE),
          stringsAsFactors=FALSE
        )
      }
    )

  # This page will also have the ids of all results for a specific distance. Grab that as well
  currentDist <-as.character(subset(distance,selected==TRUE)$distance)
  distanceIds <- list()
  distanceIds[[currentDist]] <-
    ldply(
      getNodeSet(x,'//table[@id="ContentPlaceHolder1_dlYears"]//a'),
      function(i) {
        data.frame(
          id=sub('GetResults\\((\\d+),\\d+\\)','\\1',xmlAttrs(i)[["onclick"]]),
          year=xmlValue(i),
          stringsAsFactors=FALSE
        )
      }
    )
  # Now get results for what we've got so far
  results <- 
    queryResultsFromUltraSignup(
      year=distanceIds[[currentDist]]$year,
      eventId=distanceIds[[currentDist]]$id,
      distance=currentDist,
      sleep=sleep
    )

  distance <- subset(distance,selected==FALSE)
  if (nrow(distance)==0) return(results)
  for (i in 1:nrow(distance)){
    currentDist <- as.character(distance$distance[i])
    x <- htmlParse(paste(rootURL,as.character(distance$url[i]),sep=''),isURL=TRUE); snooze(sleep)

    distanceIds[[currentDist]] <- 
      ldply(
        getNodeSet(x,'//table[@id="ContentPlaceHolder1_dlYears"]//a'),
        function(i) {
          data.frame(
            id=sub('GetResults\\((\\d+),\\d+\\)','\\1',xmlAttrs(i)[["onclick"]]),
            year=xmlValue(i),
            stringsAsFactors=FALSE
          )
        }
      )
    results <- 
      rbind(
        results,
        queryResultsFromUltraSignup(
          year=distanceIds[[currentDist]]$year,
          eventId=distanceIds[[currentDist]]$id,
          distance=currentDist,
          sleep=sleep
        )
      )
  }

  results$eventname <- eventName
  results$eid <- eid
  results
}
# queryResultsFromUltraSignup:
#
# The following information was gleaned from ultrasignup.com on October 7, 2013.

# At the time the site used jqgrid in the browser and results were pulled via
# AJAX in JSON format. Event ids were culled by noting the URL constructed
# for each AJAX call. Example URL:

# http://ultrasignup.com/service/events.svc/results/18998/json?_search=false&nd=1381181620523&rows=1000&page=1&sidx=&sord=asc
queryResultsFromUltraSignup <- 
   function 
   (
      year=    c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013),
      eventId= c(9308,9307,9306,9305,9304,9303,306,5627,6634,12848,14802,18998),
      distance='50km',
      sleep=0

   )
{
  urlTmpl = 'http://ultrasignup.com/service/events.svc/results/%d/json?_search=false'

  results <- list()

  # Data collection

  if (is.character(year)) year <- as.integer(year)
  if (is.character(eventId)) eventId <- as.integer(eventId)

  for (i in 1:length(year)){

    snooze(sleep)
    con <- url(sprintf(urlTmpl,eventId[i]))
    x <- fromJSON(file=con)
    close(con)

    if (length(x)==0){
      results[[i]] <- data.frame()
      next
    }


    # Treat all NULL values as empty strings. This will allow us to use unlist 
    # which unfortunately would drops NULL values.
    y <- lapply(x,function(i) lapply(i,function(o) if (is.null(o)) "" else o) )

    # TODO: check if there are really no results for this year, or if a network
    # error occurred
    if (length(y) <= 0) next
    colNames <- names(y[[1]])
    numRows <- length(y)
    z <- unlist(y)
    dim(z) <- c(length(colNames),numRows)
    zz <- as.data.frame(t(z),stringsAsFactors = FALSE)
    names(zz) <- colNames
    zz$year <- year[i]
    zz$event_distance_id <- eventId[i]

    results[[i]] <- zz
  }

  res <- results[[1]]
  if (length(results)>1){
    for (i in 2:length(results)){
      res <- rbind(res,results[[i]])
    }
  }

  if (nrow(res)==0)
    return(data.frame())

  # Data cleaning 

  res$age <- as.integer(res$age)
  res$age[which(res$age==0)] <- NA
  res$bib <- as.integer(res$bib)
  res$gender_place <- as.integer(res$gender_place)
  res$place <- as.integer(res$place)
  res$runner_rank <- as.numeric(res$runner_rank)
  res$time <- as.numeric(res$time) / 1000

  res$gender[which(res$gender=="f")] <- "F"
  res$gender <- factor(res$gender,levels=c("M","F"),labels=c("Men","Women"))


  # New Data

  res$time_hour <- res$time/60/60

  for (i in seq(20,75,by=5)){
    if(i == 20){
      label <- "<20"
      idx <- which(res$age < 20)
      if (length(idx) > 0)
        res[idx,'agegroup'] <- label
    } else if (i == 75){
      label <- ">=75"
      idx <- which(res$age >= 75)
      if (length(idx) > 0)
        res[idx,'agegroup'] <- label
    } else {
      label <- paste(i,'-',i+4,sep='')
      idx <- which(res$age >= i & res$age <=i+4)
      if (length(idx) > 0)
        res[idx,'agegroup'] <- label
    }
  }

  res$distance <- distance

  res
} 
