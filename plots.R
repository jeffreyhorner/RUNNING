library(brew)
library(plyr)
library(grid)
library(ggplot2)
library(knitr)
library(markdown)

createReport <- function(reportFile='Report_template.Rmd',fancybox=TRUE,template=FALSE,uploadToS3=FALSE){

  mdOpts <- markdownHTMLOptions(default=TRUE)
  mdOpts <- mdOpts[!mdOpts %in% 'base64_images']
  if (is.logical(template) && !template)
    mdOpts <- c(mdOpts,'fragment_only')

  if (is.character(template) || (is.logical(template) && template)) {
    if (is.character(template))
      templateFile <- template
    else 
      templateFile <- getOption('RUNNING.html.template')

    template <- TRUE
  }

  knitEnv <- new.env()

  if (fancybox){
    old_plot <- knit_hooks$get('plot')
    on.exit(knit_hooks$set(plot=old_plot))
    knit_hooks$set(plot=function(x,options){
      figFile <- paste(x,collapse='.')

      # Upload to S3
      if (uploadToS3){
        require(RAmazonS3)
        figFile <- sub('^/','',figFile)
        content <- readBin(figFile,raw(),file.info(figFile)[1,'size'])
        addFile(content,getOption('RUNNING.AmazonS3.bucket'),figFile,type="image/png")
        urlPrefix <- getOption('RUNNING.figure.url.prefix')
        urlPrefix <- sub('/$','',urlPrefix)
        plotUrl <- paste(urlPrefix,figFile,sep='/')
      } else {
        plotUrl <- figFile
      }
      html <- '<a class="fancybox" rel="group" href="%s"><img src="%s" alt="%s" width="500"/></a>'
      ret <- sprintf(html,plotUrl,plotUrl,ifelse(!is.null(options$fig.cap),options$fig.cap,""))
      ret
    })
  }

  # Force device to png
  old_dev <- opts_chunk$get('dev')
  on.exit(opts_chunk$set(dev=old_dev),add=TRUE)
  opts_chunk$set(dev='png')

  oldwd <- setwd(dirname(reportFile))
  on.exit(setwd(oldwd),add=TRUE)

  reportFile <- basename(reportFile)

  fileFrag <- strsplit(reportFile,'\\.')[[1]]
  mdFile <- paste(fileFrag[1],'md',sep='.')
  htmlFile <- paste(fileFrag[1],'html',sep='.')
  knit(reportFile,envir=knitEnv)
  if (template)
    markdownToHTML(mdFile,htmlFile,options=mdOpts,template=templateFile)
  else
    markdownToHTML(mdFile,htmlFile,options=mdOpts)
  
  invisible(knitEnv)
}

brewReport <- function(reportFile='Report_template.brew'){

  oldwd <- setwd(dirname(reportFile))
  on.exit(setwd(oldwd),add=TRUE)

  reportFile <- basename(reportFile)

  fileFrag <- strsplit(reportFile,'\\.')[[1]]
  htmlFile <- paste(fileFrag[1],'html',sep='.')
  brew(reportFile,htmlFile)
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
##
## Lifted from www.cookbook-r.com/Manipulating_data/Summarizing_data on
## October 10, 2013
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE,summFun=NULL) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    if (missing(summFun)){
      summFun <- function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      }
    }
    datac <- ddply(data, groupvars, .drop=.drop, .fun = summFun, measurevar)

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

createFinishersFreqDataFrame <- function(results){
  fin1 <- count(results,vars=c('year','gender'))
  fin2 <- count(fin1,vars='year') # Total

  finishers <- rbind(fin1,data.frame(year=fin2$year,gender="Total",freq=fin2$freq))

  names(finishers) <- c('year','groups','freq')

  levels(finishers$groups) <- c("Men","Women","Total")

  finishers
}

# Order is Men, Women, Total
genderColors <- c("#619CFF","#F8766D","#00BA38")

createFinishersFreqPlot <- function(finishers){
  ggplot(
    data=finishers, 
    aes(x=factor(year), y=freq, group=groups,colour=groups)
  ) + 
  geom_line() + geom_point() +
  ylab("Number of Finishers") + xlab("Year") +
  guides(colour=guide_legend(title=NULL)) +
  scale_colour_manual(values=genderColors)
}

createFinishersFreqByTimePlot <- function(results){
  rr <- range(results$time_hour)

  ggplot(data=results,aes(x=time_hour,fill=gender)) + 
  facet_grid(gender ~ .) + 
  guides(fill=FALSE) +
  geom_bar(stat="bin",binwidth=.5,position="dodge",colour="black") + 
  scale_x_continuous(breaks=seq(floor(rr[1]-1),ceiling(rr[2]+1),by=1)) +
  scale_fill_manual(values=genderColors[1:2]) +
  xlab("Time (hours)") + ylab("Number of Finishers")
}

createAgeGroupDistDataFrame <- function(results){
  age1 <- count(results,vars=c('age','gender'))
  groups <- levels(results$gender)

  agedist <- data.frame(age_group=character(),group=character(),freq=integer())

  for (i in seq(20,75,by=5)){
    if(i == 20){
      label <- "<20"
      for (j in groups) {
        freq <- sum(subset(age1,gender==j & age<i & age!=0,freq )$freq)
        agedist <- rbind(agedist,data.frame(age_group=label,group=j,freq=freq))
      }
    } else if (i == 75){
      label <- ">=75"
      for (j in groups){
        freq <- sum(subset(age1,gender==j & age>=i,freq )$freq)
        agedist <- rbind(agedist,data.frame(age_group=label,group=j,freq=freq))
      }
    } else {
      label <- paste(i,'-',i+4,sep='')
      for (j in groups){
        freq <- sum(subset(age1,gender==j & age>=i & age<=i+4,freq )$freq)
        agedist <- rbind(agedist,data.frame(age_group=label,group=j,freq=freq))
      }
    }
  }
  agedist
}

createAgeGroupDistPlot <- function(ageDist){
  ggplot(
    data=ageDist,
    aes(x=age_group,y=freq,group=group,fill=group)
  ) +
  geom_bar(stat="identity",colour="black",width=1) +
  facet_grid(group ~ .) + 
  xlab("Age (years)") + ylab("Number of Finishers") +
  guides(fill=FALSE) +
  scale_fill_manual(values=genderColors[1:2])
}

createAgeOverYearsPlot <- function(results){
  ggplot(
    data=results,
    aes(x=factor(year),y=age,color=gender)
  ) +
  geom_point(position="jitter",alpha=.5) +
  geom_boxplot(color="black", outlier.shape=NA,fill="transparent") +
  facet_grid(gender ~ .) + 
  theme(legend.position="none") +
  xlab("Year") + ylab("Age (years)") +
  guides(fill=guide_legend(title=NULL)) +
  scale_color_manual(values=genderColors[1:2])
}

createFinishTimeOverYearsPlot <- function(results,boxplot=TRUE,highlight.mean=FALSE,jitter.alpha=0.5,jitter.width=0.4,point.size=2){

  if (boxplot)
    bpObject <- geom_boxplot(color="black", outlier.shape=NA,fill="transparent")
  else 
    bpObject <- NULL

  if (highlight.mean)
    hlmObject <- stat_summary(fun.y=mean,geom="point",color="black",size=3)
  else
    hlmObject <- NULL

  rr <- range(results$time_hour)
  rr <- c(floor(rr[1]-1),ceiling(rr[2]+1))
  ggplot(data=results,aes(x=factor(year),y=time_hour,color=gender)) + 
  geom_point(position=position_jitter(width=jitter.width),alpha=jitter.alpha,size=point.size) +
  bpObject + hlmObject +
  facet_grid(gender ~ .) + 
  theme(legend.position="none") +
  scale_color_manual(values=genderColors[1:2]) +
  scale_y_continuous(breaks=seq(rr[1],rr[2],by=1),minor_breaks = seq(rr[1]+.25,rr[2]+.25,by=.25)) +
  xlab("Year") + ylab("Time (hours)")
}

createFinishTimeOverAgeGroupsPlot <- function(results,boxplot=TRUE,highlight.mean=FALSE,jitter.alpha=0.5,jitter.width=0.4,point.size=2){
  if (boxplot)
    bpObject <- geom_boxplot(color="black", outlier.shape=NA,fill="transparent")
  else 
    bpObject <- NULL

  if (highlight.mean)
    hlmObject <- stat_summary(fun.y=mean,geom="point",color="black",size=3)
  else
    hlmObject <- NULL

  rr <- range(results$time_hour)
  rr <- c(floor(rr[1]-1),ceiling(rr[2]+1))
  results$agegroup <- factor(results$agegroup,exclude="")
  ggplot(data=results,aes(x=agegroup,y=time_hour,color=gender)) + 
  geom_point(position=position_jitter(width=jitter.width),alpha=jitter.alpha,size=point.size) +
  bpObject + hlmObject +
  facet_grid(gender ~ .) + 
  theme(legend.position="none") +
  scale_color_manual(values=genderColors[1:2]) + 
  scale_y_continuous(breaks=seq(rr[1],rr[2],by=1),minor_breaks = seq(rr[1]+.5,rr[2]+.5,by=1)) +
  xlab("Age Groups") + ylab("Time (hours)")
}

createMeanTimesByYearWithErrorBarsPlot <- function(results){
  meanTimes <- summarySE(results,measurevar='time_hour',groupvars=c("year","gender"))
  pd <- position_dodge(.1)

  ggplot(meanTimes,aes(x=factor(year),y=time_hour,group=gender,colour=gender)) +
  scale_colour_manual(values=genderColors[1:2]) + 
  guides(colour=guide_legend(title=NULL)) + 
  geom_errorbar(
    aes(ymin=time_hour-sd, ymax=time_hour+sd),
    width=.3,colour="black", alpha=.3,position=pd
  ) +
  geom_line(position=pd) +
  geom_point(position=pd,size=4) +
  xlab("Year") + ylab("Average Time (hours)")
}

createMeanTimesOverAgeGroupsPlot <- function(results){
  meanTimes <- summarySE(results,measurevar='time_hour',groupvars=c("agegroup","gender"))
  pd <- position_dodge(.1)
  rr <- range(meanTimes$time_hour)
  rr <- c(floor(rr[1]-1),ceiling(rr[2]+1))
  ggplot(meanTimes,aes(x=factor(agegroup),y=time_hour,group=gender,colour=gender)) + 
  guides(colour=guide_legend(title=NULL)) + 
  geom_errorbar(
    aes(ymin=time_hour-sd, ymax=time_hour+sd),
    width=.3,colour="black", alpha=.3,position=pd
  ) +
  geom_line(position=pd) + geom_point(position=pd,size=4) + 
  scale_colour_manual(values=genderColors[1:2]) + 
  scale_y_continuous(breaks=seq(rr[1],rr[2],by=1),minor_breaks = seq(rr[1]+.5,rr[2]+.5,by=1)) +
  xlab("Age Groups") + ylab("Average Time (hours)")
}

createMeanAgeOverYearsPlot <- function(results){
  meanAge <- summarySE(results,measurevar='age',groupvars=c("year","gender"),na.rm=TRUE)
  pd <- position_dodge(.1)

  ggplot(meanAge,aes(x=factor(year),y=age,group=gender,colour=gender)) +
  scale_colour_manual(values=genderColors[1:2]) + 
  guides(colour=guide_legend(title=NULL)) + 
  geom_errorbar(
    aes(ymin=age-sd, ymax=age+sd),
    width=.3,colour="black", alpha=.3,position=pd
  ) +
  geom_line(position=pd) +
  geom_point(position=pd,size=4) +
  xlab("Year") + ylab("Average Age (hours)")
}


createElevationPlot <- function(course,startAt=-Inf,endAt=Inf,meters=TRUE,polyGrp=2){
  course <- courseSegment(course,startAt,endAt,meters)

  if (length(course$courseNames)>1)
    facetObj <- facet_grid(Course ~ .)
  else
    facetObj <- NULL

  offsetUnit <- attr(course$elevation,'offsetUnit')
  if (meters){
    xlabObj <- xlab("Distance (kilometers)")
    if (unitInMeters(offsetUnit)){
      xlabFun <- function(x) x/1000
      xscaleObj <- scale_x_continuous(breaks=seq(0,50*1000,by=1000),label=xlabFun)
    } else {
      xlabFun <- function(x) round((x * offsetUnit$toMeters)/1000)
      xscaleObj <- scale_x_continuous(breaks=seq(0,50*1000*offsetUnit$toFeet,by=1000*offsetUnit$toFeet),label=xlabFun)
    }
  } else {
    xlabObj <- xlab("Distance (miles)")
    if (unitInMeters(offsetUnit)){
      xlabFun <- function(x) (x / offsetUnit$toFeet)/5280
      xscaleObj <- scale_x_continuous(breaks=seq(0,(30*5280) * offsetUnit$toMeters,by=5280*offsetUnit$toMeters),label=xlabFun)
    } else {
      xlabFun <- function(x) x/5280
      xscaleObj <- scale_x_continuous(breaks=seq(0,30*5280,by=5280),label=xlabFun)
    }
  }

  # Elevation object
  elObj <- course$elevation

  # polygon data
  grade <- paste(attr(elObj,'grade')*100,'%',sep='')
  polys <- subset(attr(elObj,'polygons'),group==2)
  polys$steepSegment <- paste(ifelse(polys$score2>=1,'Ascent >=','Descent >='),grade)

  #  facet_grid(…, scales = "free_y", shape = "free")
  ggplot(data=elObj,aes(x=offset,y=alt,group=Course)) + 
  ylab("Altitude (feet)") + xlabObj + xscaleObj +
  facetObj +
  geom_line() +
  scale_fill_manual(values=c('red','blue')) +
  scale_colour_manual(values=c('red','blue')) +
  guides(colour=FALSE,fill=guide_legend(title="Grade")) +
  theme(legend.position = "top") +
  geom_polygon(data=polys,aes(x=x,y=y,color=steepSegment,fill=steepSegment,group=id),inherit.aes=FALSE,alpha=.8)
}

createGradeDistPlot <- function(course){
  course <- calcAscentsDescents(course)
  if (length(course$courseNames)>1)
    facetObj <- facet_grid(Course ~ .)
  else
    facetObj <- NULL
  ggplot(data=course$elevation,aes(x=grade)) +
  facetObj +
  geom_bar(stat="bin",binwidth=.03,position="dodge",colour="black")
}

createRouteMap <- function(loc='Nashville, TN',routes){
  require(ggmap)
  p <- qmap('Nashville, TN',zoom=12,source='stamen',maptype='toner-lite')
  for (i in unique(routes$name)){
    dat <- subset(routes,name==i)
    dat$lat <- jitter(dat$lat,amount=.0001)
    dat$lon <- jitter(dat$lon,amount=.0001)
    p <- p + geom_path(aes(x=lon,y=lat),data=dat,color="#0000FF",alpha=.2)
  }
  p
}
