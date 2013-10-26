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

  structure(ret, class=c("raCourseList","list"))
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

newOffsetUnit <- function(meters=TRUE) {
  # By default, RunningAHEAD elevation data are in meters and offfsets are 50 meters apart.
  # coefficients were obtained by running the UNIX command 'units' with from=meters and to=feet
  m2f <- list(unit='meters',length=50,toFeet=3.2808399,toMeters=0.3048)
  if (!meters){
    m2f$unit <- 'feet';   m2f$length <- m2f$length * m2f$toFeet
  }
  m2f
}

unitInMeters <- function(offsetUnit) offsetUnit$unit=='meters'
unitInFeet <- function(offsetUnit) !unitInMeters(offsetUnit)

# RunningAHEAD elevation data contains two columns, offsets and altitudes. offsets start at 0, increase in value,
# and are equally spaced. offset length and unit of measure are defined in the elevation attribute offsetUnit.
courseElevation <- function(xmlObj,courseName,meters=TRUE){

  offsetUnit <- newOffsetUnit(meters)

  x <- lapply(
        courseName, 
        function(i) {
          xpath <- sprintf("//CourseCollection/Course[Name='%s']/Route/Elevations/Elevation",i)
            d <- data.frame(
              offset=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'offset')),
              alt=as.numeric(sapply(getNodeSet(xmlObj,xpath),xmlGetAttr,'alt'))
            )
            d$Course <- i

          if(!all(diff(d$offset)==50)){
            oMin <- min(diff(d$offset))
            oMax <- max(diff(d$offset))
            stop("Assert failed! Presumed offset distance is not 50. min=",oMin," max=",oMax)
          }

          if (!meters){
            d$offset <- d$offset * offsetUnit$toFeet
            d$alt <- d$alt * offsetUnit$toFeet
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
      offsetUnit=offsetUnit
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
      unit=attr(elObj,"offsetUnit")$unit
  )
}

calcAscentsDescents <- function(course,grade=.15,flatness=.03,maxGap=150,minRun=800,meters=TRUE,debug=FALSE){
  if (!is(course,'raCourseList')) stop("Need object of raCourseList")

  offsetUnit <- attr(course$elevation,'offsetUnit')

  if (unitInMeters(offsetUnit) && !meters){
    maxGap <- maxGap / offsetUnit$toMeters
    minRun <- minRun / offsetUnit$toMeters
  } else if (unitInFeet(offsetUnit) && meters){
    maxGap <- maxGap * offsetUnit$toFeet
    minRun <- minRun * offsetUnit$toFeet
  }

  # Convert distances to number of offsets
  gapLen <- as.integer(round(maxGap/offsetUnit$length))
  runLen <- as.integer(round(minRun/offsetUnit$length))

  if (length(grade)>=2) {
    aGrade <- grade[1]; dGrade <- grade[2]
  } else {
    aGrade <- grade; dGrade <- -aGrade
  }
  if (length(flatness)>=2) {
    upFlat <- flatness[1]; downFlat <- flatness[2]
  } else {
    upFlat <- flatness; downFlat <- -upFlat
  }

  scoreCourse <- function(kourse){
    elObj <- subset(course$elevation,Course==kourse)

    # For each offset, compute a score in the range c(-1,1) as a measure of difficulty based
    # on elevation grade (e.g. slope of the line segment). Negative scores define descending grades
    # while positive scores define ascending grades. Scores at the extremes define the offset as
    # being outside of the grade range provided by the argument grade. For instance if the argument is .15
    # offset grades equal to or greater than .15 get assigned 1, and offset grades equal to or less than -.15
    # get assigned -1. If the grade argument is length two, then the grade range is defined explicitely by the
    # two values.

    # Compute the difference between adjacent altitude readings and store
    # back in the DF. Only needed right now to compute the percent grade, but
    # may be needed later. Last reading is zero, since it makes sense that one
    # really doesn't care how high up you are once your at the end of the course.
    # Makes more sense to know, at a particular offset on the course what you
    # are about to encounter.
    elObj$altDelta <- c(diff(elObj$alt),0)

    elObj$grade <- elObj$altDelta / offsetUnit$length


    # Grades that are at or above/below the grade range get 
    # scores of 1 or -1. This will create runs of non-zero score segments
    elObj$score <- 0.0
    elObj$score[which(elObj$grade>=aGrade)] <- 1
    elObj$score[which(elObj$grade<=dGrade)] <- -1

    # Now find zero score runs of length gapLen or less that exist between non-zero score segments
    # and change them to non-zero if they are not flat (defined by flatness argument).
    inRun <- FALSE
    zeroRunLen <- 0

    isZero <- function(x) unlist(lapply(x,function(i)isTRUE(all.equal(i,0))))
    
    for (i in 1:nrow(elObj)){
      if (all(isZero(elObj$score[i]))){
        inRun <- TRUE
        zeroRunLen <- zeroRunLen + 1
      } else {
        if (inRun && zeroRunLen <= gapLen){
          runIdx <- seq(i-zeroRunLen,i-1)
          for (j in runIdx){
            # Grades that are just below/above the threshold and are not measured as flat
            # get the second highest score.
            if (elObj$grade[j] > upFlat)
              elObj$score[j] <- .5
            else if (elObj$grade[j] <- downFlat)
              elObj$score[j] <- -.5
          }
        }
        inRun <- FALSE
        zeroRunLen <- 0
      }
    }

    # Magic be here
    elObj$adStart <- 0L
    elObj$adEnd <- 0L
    elObj$isZero <- isZero(elObj$score)
    z <- which(isZero(elObj$score))
    y <- diff(z)
    elObj$adStart[z[y>runLen]] <- 1
    elObj$adEnd[z[y>runLen] + y[y>runLen]] <- 1


    elObj
  }

  if (debug){

    smoothCourse <- function(kourse){
      elObj <- subset(course$elevation,Course==kourse)
      lObj <- loess(alt ~ offset, elObj, span=.01)
      elObj$alt <- predict(lObj)
      elObj$Course <- paste(kourse,'smooth')

      elObj
    }

    x <- lapply(course$courseNames,smoothCourse)
    z <- x[[1]]
    if (length(x) > 1){
      for (i in 2:length(x)){
        z <- rbind(z,x[[i]])
      }
    }
    
    offsetUnit <- attr(course$elevation,'offsetUnit')
    course$elevation <- rbind(course$elevation,z)
    course$courseNames <- unique(c(course$courseNames,z$Course))
    attr(course$elevation,'offsetUnit') <- offsetUnit

  }

  x <- lapply( course$courseNames,scoreCourse)
  z <- x[[1]]
  if (length(x) > 1){
    for (i in 2:length(x)){
      z <- rbind(z,x[[i]])
    }
  }

  attr(z,'offsetUnit') <- attr(course$elevation,'offsetUnit')
  attr(z,'flatness') <- flatness
  attr(z,'grade') <- grade
  attr(z,'runLen') <- runLen
  attr(z,'gapLen') <- gapLen

  course$elevation <- z

  course
}

courseSegment <- function(course, startAt=-Inf, endAt=Inf, meters=TRUE){
  offsetUnit <- attr(course$elevation,'offsetUnit')

  if (unitInMeters(offsetUnit) && !meters){
    startAt <- startAt / offsetUnit$toMeters
    endAt <- endAt / offsetUnit$toMeters
  } else if (unitInFeet(offsetUnit) && meters){
    startAt <- startAt * offsetUnit$toFeet
    endAt <- endAt * offsetUnit$toFeet
  }

  elevAttr <- attributes(course$elevation)

  course$elevation <- subset(course$elevation,offset>=startAt&offset<=endAt)
  attr(course$elevation,'offsetUnit') <- elevAttr$offsetUnit

  course
}
