mileSplits <- function(splits=c(),target='',distance=13.1){
    targetN <- str2sec(target)
    avgPace <- targetN/distance
    numMiles <- floor(distance)
    remainderDist <- distance - numMiles
    remainderSec <- targetN - (avgPace*numMiles)

    EvenSplits <- sec2str(c(rep(avgPace,numMiles),remainderSec))
    EvenCum <- sec2str(cumsum(str2sec(EvenSplits)))
    if (length(splits)){
	splitsN <- str2sec(splits)
	numMilesToGo <- numMiles - length(splits)
	newTarget <- targetN - sum(splitsN)
	newDistance <- distance - length(splits)
	newAvgPace <- newTarget/newDistance
	newRemainderSec <- newTarget - (newAvgPace*numMilesToGo)

	CalcSplits <- sec2str(c(splitsN,rep(newAvgPace,numMilesToGo),newRemainderSec))
	CalcCum <- sec2str(cumsum(str2sec(CalcSplits)))
	data.frame(EvenSplits=EvenSplits,EvenCum=EvenCum,CalcSplits=CalcSplits,
	       CalcCum=CalcCum)
    } else {
	data.frame(EvenSplits=EvenSplits,EvenCum=EvenCum)
    }

}

toSeconds <- function(x){
   if (!is.character(x)) stop("x must be a character string of the form H:M:S")
   if (length(x)<=0)return(x)

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
}
str2sec <- toSeconds

secondsToString <- function(x,digits=2){
   unlist(
      lapply(x,
         function(i){
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
            if (fs > 0)
               sub('[0]+$','',paste(i,fs,sep='.'))
            else
               i
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
