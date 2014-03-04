library(datasets)
source('ultrasignup.R')

eventExists <- function(q=NULL,eid=NULL){
  if (!is.null(q)){
    ultraURL <- sprintf('http://ultrasignup.com/service/events.svc/GetFeaturedEventsSearch/p=0/q=%s',URLencode(q,reserve=TRUE))
    return(fromJSONDF(ultraURL))
  }

  if (!is.null(eid)){
    return(adply(
      as.integer(eid),
      .margins=1,
      .fun=function(i){
        rootURL <- 'http://ultrasignup.com/'
        ultraURL <- sprintf('http://ultrasignup.com/register.aspx?eid=%d',i)
        # ultraURL <- "http://ultrasignup.com/register.aspx?did=23884"
        # From the registration page, gather all time ID's:
        # for
        x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE);

        if (xmlValue(getNodeSet(x,'//title')[[1]])=='Object moved'){
          ultraURL <- paste('http://ultrasignup.com',xmlAttrs(getNodeSet(x,'//a')[[1]]),sep='')
          # Event Id unknown
          if (rootURL == ultraURL) return(data.frame())
          x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE);
        }
        # getNodeSet(x,'//table[@id="ContentPlaceHolder1_Results11_dlResultYears"]//a')
        timeId <- ldply(getNodeSet(x,'//table[@id="ContentPlaceHolder1_Results11_dlResultYears"]//a'),function(i) data.frame(url=xmlAttrs(i)[["href"]],year=xmlValue(i)))
        if (nrow(timeId) == 0) return(data.frame())

        eventName <- getNodeSet(x,'//h1[@class="event-title"]',fun=xmlValue)[[1]]

        data.frame(name=eventName,eid=i,stringsAsFactors=FALSE)
      }
    ))
  }

  data.frame()
}

eventsByState <- function(){
  data(state)

  events <- data.frame() 
  for (i in state.name){
    x <- queryEventByLoc(i)
    events <- rbind(events,x)
    events <- unique(events)
  }
  events
}
