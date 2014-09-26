library(plyr)
# assumes pace and distance in miles
timeSplits <- function(splitType=c('distance','time'),splitsEvery=c(),splitsAt=c(),target='',distance=13.1,pace='',splitNames=NULL){
  if (nzchar(target) && nzchar(pace) || !nzchar(target) && !nzchar(pace))
    stop('Choose either target or pace')

  
  if (nzchar(target)){
    Target <- str2sec(target)
    Pace <- Target/distance
  } else {
    Pace <- str2sec(pace)
    Target <- Pace * distance
  }
  
  # splits in distance
  if (length(splitsEvery) || length(splitsAt)){

    if (length(splitsEvery))
      splitsAt <- seq(splitsEvery,distance,by=splitsEvery)

    Distances <- c(splitsAt[1],diff(splitsAt))

    remainderDist <- distance - sum(Distances)
    if (remainderDist < 0)
      stop('splitsAt distance too long')
    Distances[length(Distances)+1] <- remainderDist
    CummDist <- 0
    CummTime <- 0
    cumFun <- function(i){
      CummDist <<- CummDist + i
      Split <- Pace * i
      CummTime <<- CummTime + Split
      data.frame(Distance=i,CummDist=CummDist, Time=sec2str(Split), CummTime=sec2str(CummTime), Pace=sec2str(Pace))
    }
    x <- adply(Distances,1,.fun=cumFun)
    x$X1 <- NULL
  } else {
    numMiles <- floor(distance)
    remainderDist <- distance - numMiles
    remainderSec <- Target - (Pace*numMiles)
    Distances <- c(rep(1,numMiles),remainderDist)

    EvenSplits <- sec2str(c(rep(Pace,numMiles),remainderSec))
    EvenCum <- sec2str(cumsum(str2sec(EvenSplits)))
    Distances <- c(rep(1,numMiles),remainderDist)
    x <- data.frame(Distance=Distances,CummDist=cumsum(Distances),Time=EvenSplits,CummTime=EvenCum,Pace=sec2str(rep(Pace,length(Distances))))
  }
  if (!missing(splitNames))
    x$Names <- c(splitNames,rep('',nrow(x)-length(splitNames)))

  x
}

# Make a class called Duration, a subclass of numeric, whose value is elapsed
# time in seconds. Hence, fractional values measure sub-second values.

print.Duration <- function(x, ...){
   print(secondsToString(x),...)
}

toSeconds <- function(x){
   if (!is.character(x) && !is(x,'Duration'))
      stop("x must be a character string of the form H:M:S")
   if (length(x)<=0)return(x)

   structure(
      unlist(
         lapply(x,
            function(i){
               i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
               if (length(i) == 3)
                  i[1]*3600 + i[2]*60 + i[3]
               else if (length(i) == 2)
                  i[1]*60 + i[2]
               else if (length(i) == 1)
                  i[1]
            }
         )
      )
      , class=c("Duration","numeric")
   )
}
str2sec <- toSeconds

options(sec2str.fractional.seconds=FALSE)
secondsToString <- function(x,digits=2){

   if (!is.numeric(x) && !is(x,'Duration'))
      stop("x must be a numeric or of class Duration")
   if (length(x)<=0)return(x)

   unlist(
      lapply(x,
         function(i){
            if (is.na(i)) return(NA)
            # fractional seconds
            fs <- as.integer(round((i - round(i))*(10^digits)))
            fmt <- ''
            if (i >= 3600)
               fmt <- '%H:%M:%S'
            else if (i >= 60)
            fmt <- '%M:%S'
            else
               fmt <- '%OS'

            i <- format(as.POSIXct(strptime("0:0:0","%H:%M:%S")) + i, format=fmt)
            if (fs > 0 && getOption('sec2str.fractional.seconds'))
               i <- sub('[0]+$','',paste(i,fs,sep='.'))
            
            sub('^0+','',i)
         }
      )
   )
}
sec2str <- secondsToString

milesToMeters <- function(miles=1){
	miles * 1609.344
}

metersToMiles <- function(meters=1){
	meters / 1609.344
}

toMPH <- function(secs=0,miles=NA,meters=NA){
	if (is.na(miles) && is.na(meters)) stop("Need miles or meters")
	miles <- ifelse (is.na(miles),miles <- metersToMiles(meters),miles)
	secs <- ifelse (is.character(secs), toSeconds(secs), secs)

	miles / (secs / 3600)
}

toKPH <- function(secs=0,miles=NA,meters=NA){
	if (is.na(miles) && is.na(meters)) stop("Need miles or meters")
	meters <- ifelse (is.na(meters),meters <- milesToMeters(miles),meters)
	secs <- ifelse (is.character(secs), toSeconds(secs), secs)

	(meters/1000) / (secs / 3600)
}

toMinPerMile <- function(secs=0,miles=NA,meters=NA){
	if (is.na(miles) && is.na(meters)) stop("Need miles or meters")
	miles <- ifelse (is.na(miles),miles <- metersToMiles(meters),miles)
	secs <- ifelse (is.character(secs), toSeconds(secs), secs)

	secondsToString(secs / miles)
}

unixunits <- function(fromU=NA,toU=NA){
	as.numeric(system(paste('units -t',fromU,toU),intern=TRUE))
}

workoutTotals <- function(runs=c(),times=c()){
    miles <- sum(runs)
    times <- sum(str2sec(times))
    structure(miles,pace=sec2str(times/miles),duration=sec2str(times))
}
