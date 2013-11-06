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

# Create polygons
polyDF <- function(x,y,id,score,score2,group,course){
    data.frame(
      x=as.numeric(x),y=as.numeric(y),id=as.integer(id),score=as.numeric(score),
      score2=as.numeric(score2), group=as.integer(group),Course=course
    )
}

calcAscentsDescents <- function(course,grade=.10,flatness=.03,maxGap=150,minRun=100,meters=TRUE,debug=FALSE){
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
    elObj$score <- elObj$score2 <- 0.0
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
    elObj$adSegment <- 0L
    elObj$isZero <- isZero(elObj$score)
    z <- which(isZero(elObj$score))
    y <- diff(z)
    adStarts <- z[which(y>runLen)] + 1
    if (length(adStarts)>0 && adStarts[length(adStarts)] > nrow(elObj))
      adStarts[length(adStarts)] <- nrow(elObj)
    adEnds <- adStarts + y[y>runLen] - 2

    if (length(adStarts)>0){
      elObj$adStart[adStarts] <- 1
      elObj$adEnd[adEnds] <- 1
      for (i in 1:length(adStarts))
        elObj$adSegment[seq(adStarts[i],adEnds[i])] <- 1

      # Score 2
      # Goal is to reduce each segment to generally ascdending/descending or undulating
      # We take score from above and score as asc/desc, find runs of length one and see
      # if we can flip them.
      elObj$score2 <- 0L
      for (i in 1:length(adStarts)){
        score2 <- ifelse(elObj$score[seq(adStarts[i],adEnds[i])] > 0.0, 1L, -1L)
        score <- rle(score2)
        scoreLen <- length(score$values)
        # Runs of length one are weak
        # See if we can flip this value if previous and next runs are opposite
        for (j in which(score$lengths==1)){
          if (j-1 > 1 && j+1 <= scoreLen){
            x <- list(val=score$values[j-1],len=score$lengths[j-1])
            y <- list(val=score$values[j],len=score$lengths[j])
            z <- list(val=score$values[j+1],len=score$lengths[j+1])
            if (y$val != x$val && y$val!= z$val && y$len < x$len && y$len < z$len)
              score2[sum(score$lengths[seq(1,j)])] <- x$val
          }
        }
        elObj$score2[seq(adStarts[i],adEnds[i])] <- score2
      }

      
      # Create summary data
      summaryDF <- function(startAt,endAt,distance,minGrade,maxGrade,meanGrade,medGrade,oaGrade,score2){
          data.frame(
            startAt=startAt,endAt=endAt,distance=distance,minGrade=minGrade,
            maxGrade=maxGrade,meanGrade=meanGrade,medGrade=medGrade,oaGrade=oaGrade,
            score2=score2, Course=elObj$Course[1]
          )
      }
        
      sumM <- NULL
      polyN <- NULL
      k <- 1L
      l <- 1L
      for (i in 1:length(adStarts)){
        for (j in seq(adStarts[i],adEnds[i])){
          xx <- elObj[c(j,j+1),'offset']
          yy <- elObj[c(j,j+1),'alt']
          polyN <- 
            rbind(polyN,
              polyDF(
                x=c(xx[1],xx[1],xx[2],xx[2]),
                y= c(-Inf,yy[1],yy[2],-Inf),
                id=k,
                score=elObj[j,'score'],
                score2=elObj[j,'score2'],
                group=1, course=kourse
              )
            )
          k <- k + 4
        }
        segIds <- seq(adStarts[i],adEnds[i])
        scRuns <- rle(elObj$score2[segIds])
        m <- 1L
        for (j in 1:length(scRuns$lengths)){
          subSegIds <- seq(segIds[m],length.out=scRuns$lengths[j]+1)
          endId <- length(subSegIds)
          xx <- elObj[subSegIds,'offset']
          yy <- elObj[subSegIds,'alt']
          ggrade <- elObj[subSegIds,'grade']
          sscore2 <- elObj[subSegIds[1],'score2']
          sumM <- 
            rbind(sumM,
              summaryDF(
                startAt=xx[1],endAt=xx[endId],distance=xx[endId]-xx[1],
                minGrade=min(ggrade),maxGrade=max(ggrade),
                meanGrade=mean(ggrade),medGrade=median(ggrade),
                oaGrade=(yy[endId]-yy[1])/(xx[endId]-xx[1]),score2=sscore2
              )
            )
                
          minY <- min(yy)
          if (sscore2==1){
            if (minY == yy[1]){
              xx <- c(xx, xx[length(xx)])
              yy <- c(yy,minY)
            } else {
              xx <- c(xx[1], xx, xx[length(xx)])
              yy <- c(minY,yy,minY)
            }
          } else {
            if (minY == yy[length(yy)]){
              xx <- c(xx[1],xx)
              yy <- c(minY,yy)
            } else {
              xx <- c(xx[1], xx, xx[length(xx)])
              yy <- c(minY,yy,minY)
            }
          }
          polyN <- 
            rbind(polyN,
              polyDF(
                x=xx,
                y= yy,
                id=l,
                score=0,
                score2=sscore2,
                group=2,
                course=kourse
              )
            )

          l <- l + 1
          m <- m + scRuns$lengths[j]
        }
      }
    } else {
      sumM <- NULL
      polyN <- NULL
    }

    attr(elObj,'polygons') <- polyN
    attr(elObj,'summary') <- sumM

    elObj
  }

  if (debug){

    smoothCourse <- function(kourse){
      elObj <- subset(course$elevation,Course==kourse)
      lObj <- loess(alt ~ offset, elObj, span=.1)
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
  y <- attr(x[[1]],'rectangles')
  w <- attr(x[[1]],'polygons')
  v <- attr(x[[1]],'summary')
  z <- x[[1]]
  if (length(x) > 1){
    for (i in 2:length(x)){
      y <- rbind(y,attr(x[[i]],'rectangles'))
      w <- rbind(w,attr(x[[i]],'polygons'))
      v <- rbind(v,attr(x[[i]],'summary'))
      z <- rbind(z,x[[i]])
    }
  }

  attr(z,'polygons') <- w
  attr(z,'summary') <- v
  attr(z,'offsetUnit') <- attr(course$elevation,'offsetUnit')
  attr(z,'flatness') <- flatness
  attr(z,'grade') <- grade
  attr(z,'runLen') <- runLen
  attr(z,'gapLen') <- gapLen

  course$elevation <- z

  course
}


courseSegment <- function(course, startAt=-Inf, endAt=Inf, meters=TRUE){
  if (isTRUE(all.equal(-Inf,startAt)) && isTRUE(all.equal(Inf,endAt)))
    return(course)

  offsetUnit <- attr(course$elevation,'offsetUnit')
  oldAttr <- attributes(course$elevation)

  if (unitInMeters(offsetUnit) && !meters){
    startAt <- startAt / offsetUnit$toMeters
    endAt <- endAt / offsetUnit$toMeters
  } else if (unitInFeet(offsetUnit) && meters){
    startAt <- startAt * offsetUnit$toFeet
    endAt <- endAt * offsetUnit$toFeet
  }

  course$elevation <- subset(course$elevation,offset>=startAt&offset<=endAt)
  polygonSegment <- function(polyA,startAt,endAt){
    nPoly <- NULL
    for (i in unique(polyA$Course)){
      cPoly <- subset(polyA,Course==i)
      for (j in unique(cPoly$group)){
        gPoly <- subset(cPoly,group==j)
        xPoly <- subset(gPoly,x>=startAt&x<=endAt)
        startPoly <- subset(gPoly,id==xPoly[1,'id'])
        endPoly <- subset(gPoly,id==xPoly[nrow(xPoly),'id'])
        nStartPoly <- nEndPoly <- NULL
        if (nrow(startPoly) != nrow(subset(xPoly,id==startPoly$id[1])))
          nStartPoly <- polyDF(x=startAt,y=min(startPoly$y),id=startPoly$id[1],score=startPoly$score[1],score2=startPoly$score2[1],group=j,course=i)
        else
          nStartPoly <- startPoly
        if (nrow(endPoly) != nrow(subset(xPoly,id==endPoly$id[1])))
          nEndPoly <- polyDF(x=endAt,y=min(endPoly$y),id=endPoly$id[1],score=endPoly$score[1],score2=endPoly$score2[1],group=j,course=i)
        else
          nEndPoly <- endPoly

        nPoly <- rbind(nPoly, nStartPoly,xPoly, nEndPoly)
      }
    }
    nPoly
  }
  for (i in names(oldAttr)){
      if (!i %in% c('names','row.names','class','dim','comment','dimnames','tsp','polygons'))
        attr(course$elevation,i) <- oldAttr[[i]]
      else if (i=='polygons')
        attr(course$elevation,'polygons') <- polygonSegment(oldAttr[['polygons']],startAt,endAt)
  }

  course
}
