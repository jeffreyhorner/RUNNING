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
