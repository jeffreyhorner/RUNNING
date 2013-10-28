library(plyr)
library(grid)
library(ggplot2)
library(knitr)
library(markdown)

createReport <- function(reportFile='Report_template.Rmd'){

  oldwd <- setwd(dirname(reportFile))
  on.exit(setwd(oldwd))

  reportFile <- basename(reportFile)

  fileFrag <- strsplit(reportFile,'\\.')[[1]]
  mdFile <- paste(fileFrag[1],'md',sep='.')
  htmlFile <- paste(fileFrag[1],'html',sep='.')
  knit(reportFile)
  markdownToHTML(
    mdFile,htmlFile,options=markdownHTMLOptions(default=TRUE),
    fragment.only=TRUE
  )
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
  ggplot(data=results,aes(x=time_hour,fill=gender)) + 
  facet_grid(gender ~ .) + 
  guides(fill=FALSE) +
  geom_bar(stat="bin",binwidth=.5,position="dodge",colour="black") + 
  scale_x_continuous(breaks=seq(3,12,by=1)) +
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
    aes(x=factor(age_group),y=freq,group=group,fill=group)
  ) +
  geom_bar(stat="identity",colour="black") +
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

  ggplot(data=results,aes(x=factor(year),y=time_hour,color=gender)) + 
  geom_point(position=position_jitter(width=jitter.width),alpha=jitter.alpha,size=point.size) +
  bpObject + hlmObject +
  facet_grid(gender ~ .) + 
  theme(legend.position="none") +
  scale_color_manual(values=genderColors[1:2]) +
  scale_y_continuous(breaks=seq(4,10,by=1),minor_breaks = seq(3.5,10.5,by=1)) +
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

  results$agegroup <- factor(results$agegroup,exclude="")
  ggplot(data=results,aes(x=agegroup,y=time_hour,color=gender)) + 
  geom_point(position=position_jitter(width=jitter.width),alpha=jitter.alpha,size=point.size) +
  bpObject + hlmObject +
  facet_grid(gender ~ .) + 
  theme(legend.position="none") +
  scale_color_manual(values=genderColors[1:2]) + 
  scale_y_continuous(breaks=seq(4,10,by=1),minor_breaks = seq(3.5,10.5,by=1)) +
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
  ggplot(meanTimes,aes(x=factor(agegroup),y=time_hour,group=gender,colour=gender)) + 
  guides(colour=guide_legend(title=NULL)) + 
  geom_errorbar(
    aes(ymin=time_hour-sd, ymax=time_hour+sd),
    width=.3,colour="black", alpha=.3,position=pd
  ) +
  geom_line(position=pd) + geom_point(position=pd,size=4) + 
  scale_colour_manual(values=genderColors[1:2]) + 
  scale_y_continuous(breaks=seq(4,10,by=1),minor_breaks = seq(3.5,10.5,by=1)) +
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


createElevationPlot <- function(course,startAt=-Inf,endAt=Inf,meters=TRUE){
  course <- courseSegment(course,startAt,endAt,meters)
  course <- calcAscentsDescents(course,grade=.10,flatness=.03,maxGap=150, minRun=100)

  if (length(course$courseNames)>1)
    facetObj <- facet_grid(Course ~ .)
  else
    facetObj <- NULL

  mile_scale <- function(x) x/5280

#  scale_fill_gradient2(low='blue',high='red') +
#  geom_polygon(data=attr(course$elevation,'polygons'),aes(x=x,y=y,fill=grade,group=id),inherit.aes=FALSE,alpha=.7)
#  geom_rect(data=attr(course$elevation,'rectangles'),aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=grade),inherit.aes=FALSE,alpha=.5) + 
  ggplot(data=course$elevation,aes(x=offset,y=alt,group=Course)) + 
  ylab("Altitude (feet)") + xlab("Distance (miles)") +
  facetObj +
  scale_x_continuous(breaks=seq(0,30*5280,by=5280),label=mile_scale) +
  geom_line() +
  scale_fill_gradient2(low='blue',high='red') +
  theme(legend.position="none") +
  geom_polygon(data=attr(course$elevation,'polygons'),aes(x=x,y=y,fill=score2,group=id),inherit.aes=FALSE,alpha=.7)
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
