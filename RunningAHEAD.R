library(XML)

readXMLTrainingLog <- function(logFile='log.xml') xmlParse(logFile)

courseNames <- function(xmlObj) sapply(getNodeSet(xmlObj,"//CourseCollection/Course/Name"),xmlValue)

getCourses <- function(xmlObj,courseName,meters=TRUE){
  ret <- list(
    waypoints = courseWayPoints(xmlObj,courseName),
    elevation = courseElevation(xmlObj,courseName,meters=meters)
  )
  ret$gainLoss <- elevationGainLoss(ret$elevation)
  ret$courseNames <- courseName

  ret
}

courseWayPoints <- function(xmlObj,courseName){
  xpath <- sprintf("//CourseCollection/Course[Name='%s']/Route/WayPoints/LatLng",courseName)
  data.frame(
    lat=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'lat')),
    lng=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'lng'))
  )
  x <- lapply(
        courseName, 
        function(i) {
          xpath <- sprintf("//CourseCollection/Course[Name='%s']/Route/WayPoints/LatLng",i)
            d <- data.frame(
              lat=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'lat')),
              lng=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'lng'))
            )
            d$Course <- i
          d
        }
      )

  z <- x[[1]]
  if (length(x) > 1){
    for (i in 2:length(x)){
      z <- rbind(z,x[[i]])
    }
  }
  structure(z, class=c("raWayPointsDF","data.frame"))
}

courseElevation <- function(xmlObj,courseName,meters=TRUE){
  x <- lapply(
        courseName, 
        function(i) {
          xpath <- sprintf("//CourseCollection/Course[Name='%s']/Route/Elevations/Elevation",i)
            d <- data.frame(
              offset=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'offset')),
              alt=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'alt'))
            )
            d$Course <- i

          if (!meters){
            d$offset <- d$offset * 3.28084
            d$alt <- d$alt * 3.28084
          }
          d
        }
      )

  z <- x[[1]]
  if (length(x) > 1){
    for (i in 2:length(x)){
      z <- rbind(z,x[[i]])
    }
  }
  structure(z,
      class=c("raElevationDF","data.frame"),
      unit=ifelse(meters,"meters","feet")
  )
}

elevationGainLoss <- function(elObj){
    if (!is(elObj,'raElevationDF')) stop("Need object of raElevationDF")
  x <- lapply(
        unique(elObj$Course), 
        function(i) {
            altDiff <- diff(subset(elObj,Course==i)$alt)
            d <- data.frame(
              Gain=sum(altDiff[which(altDiff > 0.0)]),
              Loss=abs(sum(altDiff[which(altDiff <= 0.0)])),
              Course=i
            )
        }
      )

  z <- x[[1]]
  if (length(x) > 1){
    for (i in 2:length(x)){
      z <- rbind(z,x[[i]])
    }
  }
  structure(z,
      class=c("raElevationGainLossDF","data.frame"),
      unit=attr(elObj,"unit")
  )
}

calcAscentsDescents <- function(elObj,grade=.15,fudge=2,fudge2=8){

  # RunningAHEAD elevation data contains offsets and altitudes. offsets always start at 0, meaning
  # element 1 is 0, and are equaly spaced. That means element 2,i.e. elObj$offset[2],
  # ontains the distance between two consecutive offsets.

  # Unit of measure for offsets and altitudes are in either meter or feet

  offsetLength <- elObj$offset[2]

  elObj$altDelta <- c(diff(elObj$alt),0)

  elObj$grade <- elObj$altDelta / offsetLength

  elObj$ascdesc <- 0.0
  elObj$ascdesc[which(elObj$grade>=grade)] <- 1
  elObj$ascdesc[which(elObj$grade<=-grade)] <- -1

  elObj$adStart <- 0L
  elObj$adEnd <- 0L

  # Now find runs of length fudge or less for ascdesc and change them to something non-zero
  inRun <- FALSE
  runLen <- 0
  for (i in 1:nrow(elObj)){
    if (elObj$ascdesc[i]==0){
      inRun <- TRUE
      runLen <- runLen + 1
    } else {
      if (inRun && runLen <= fudge){
        runIdx <- seq(i-runLen,i-1)
        for (j in runIdx)
          elObj$ascdesc[j] <- if(elObj$grade[j]>=0.0) .5 else -.5 
      }
      inRun <- FALSE
      runLen <- 0
    }
  }

  # Magic be here
  z <- which(elObj$ascdesc == 0.0)
  y <- diff(z)
  elObj$adStart[z[y>fudge2]] <- 1
  elObj$adEnd[z[y>fudge2] + y[y>fudge2]] <- 1

  attr(elObj,'grade') <- grade
  attr(elObj,'fudge') <- fudge

  elObj
}
