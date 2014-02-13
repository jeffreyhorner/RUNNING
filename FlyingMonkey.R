library(XML)
library(plyr)

fun2006 <- function(i){
    iLen <- length(i)
    if (i[1]=="Rosenbloom")
      quickdf(list(lastname=i[1],firstname=paste(i[2],i[3]),gender=i[4],age=i[5],bib=i[6],time=i[7],oa=i[8],ag=i[9],city=i[10],state=i[11],country=i[12]))
    else if (i[1]=="Schlamp")
      quickdf(list(lastname=i[1],firstname=i[2],gender=i[3],age=i[4],bib=i[5],time=i[6],oa=i[7],ag=i[8],city=i[9],state=NA,country=i[10]))
    else if (iLen==11)
      quickdf(list(lastname=i[1],firstname=i[2],gender=i[3],age=i[4],bib=i[5],time=i[6],oa=i[7],ag=i[8],city=i[9],state=i[10],country=i[11]))
    else if (iLen==12)
      quickdf(list(lastname=i[1],firstname=i[2],gender=i[3],age=i[4],bib=i[5],time=i[6],oa=i[7],ag=i[8],city=paste(i[9],i[10]),state=i[11],country=i[12]))
    else if (iLen==10)
      quickdf(list(lastname=i[1],firstname=i[2],gender=i[3],age=i[4],bib=i[5],time=NA,oa=NA,ag=NA,city=paste(i[7],i[8]),state=i[9],country=i[10]))
    else if (iLen==9)
      quickdf(list(lastname=i[1],firstname=i[2],gender=i[3],age=i[4],bib=i[5],time=NA,oa=NA,ag=NA,city=i[7],state=i[8],country=i[9]))
}
fun2007 <- function(i){
    iLen <- length(i)
    if (i[2]=='Leaver')
      quickdf(list(bib=i[1],lastname=i[2],firstname='Christopher',gender='M',age=i[5],city=i[6],state=i[7],time=i[8],oa=i[9],ag=i[1],country=NA))
    else if (i[2]=='Van' && i[3]=='Sise')
      quickdf(list(bib=i[1],lastname=paste(i[2],i[3]),firstname=i[4],gender=i[5],age=i[6],city=i[7],state=i[8],time=i[9],oa=i[10],ag=i[11],country=NA))
    else if (iLen==10)
      quickdf(list(bib=i[1],lastname=i[2],firstname=i[3],gender=i[4],age=i[5],city=i[6],state=i[7],time=i[8],oa=i[9],ag=i[1],country=NA))
    else if (iLen==11)
      quickdf(list(bib=i[1],lastname=i[2],firstname=i[3],gender=i[4],age=i[5],city=paste(i[6],i[7]),state=i[8],time=i[9],oa=i[10],ag=i[11],country=NA))
    else if (iLen==12)
      quickdf(list(bib=i[1],lastname=i[2],firstname=i[3],gender=i[4],age=i[5],city=paste(i[6],i[7],i[8]),state=i[9],time=i[10],oa=i[11],ag=i[12],country=NA))
    else if (iLen==9)
      quickdf(list(bib=i[1],lastname=i[2],firstname=i[3],gender=i[4],age=i[5],city=NA,state=NA,time=i[7],oa=i[8],ag=i[9],country=i[6]))
    else if (iLen==8)
      quickdf(list(bib=i[1],lastname=i[2],firstname=i[3],gender=i[4],age=i[5],city=i[6],state=i[7],time=NA,oa=NA,ag=NA,country=NA))
}
fun2008 <- function(i){
    iLen <- length(i)
    if (i[5] %in% c('Troy','Cathie') && i[6]=='Johnson'){
      quickdf(list(bib=i[1],time=i[2],oa=i[3],ag=i[4],firstname=i[5],lastname=i[6],gender=i[7],age=i[8],city='Red Boiling Springs',state='TN',country=NA))
    } else if ((i[5] == 'Matthew' && i[6]=='Boschen') || (i[5]=='Klaus' && i[6]=='Westphal')){
      quickdf(list(bib=i[1],time=i[2],oa=i[3],ag=i[4],firstname=i[5],lastname=i[6],gender=i[7],age=i[8],city=i[9],state=NA,country=i[10]))
    } else if (i[5] %in% c('Keith','Andrea')){
      quickdf(list(bib=i[1],time=i[2],oa=i[3],ag=i[4],firstname=i[5],lastname=paste(i[6],i[7]),gender=i[8],age=i[9],city=i[10],state=i[11],country=NA))
    } else if(grepl('^infinity',i[1])){
      i[1] <- sub('^infinity','',i[1])
      quickdf(list(bib=Inf,time=i[1],oa=i[2],ag=i[3],firstname=i[4],lastname=i[5],gender=i[6],age=i[7],city=i[8],state=i[9],country=NA))
    } else if (iLen==9){
      quickdf(list(bib=i[1],time=NA,oa=NA,ag=NA,firstname=i[3],lastname=i[4],gender=i[5],age=i[6],city=paste(i[7],i[8]),state=i[9],country=NA))
    } else if (iLen==8){
      quickdf(list(bib=i[1],time=NA,oa=NA,ag=NA,firstname=i[3],lastname=i[4],gender=i[5],age=i[6],city=i[7],state=i[8],country=NA))
    } else if (iLen==10) {
      quickdf(list(bib=i[1],time=i[2],oa=i[3],ag=i[4],firstname=i[5],lastname=i[6],gender=i[7],age=i[8],city=i[9],state=i[10],country=NA))
    } else if (iLen==11) {
      quickdf(list(bib=i[1],time=i[2],oa=i[3],ag=i[4],firstname=i[5],lastname=i[6],gender=i[7],age=i[8],city=paste(i[9],i[10]),state=i[11],country=NA))
    } else if (iLen==9) {
      quickdf(list(bib=i[1],time=i[2],oa=i[3],ag=i[4],firstname=i[5],lastname=i[6],gender=i[7],age=i[8],city=paste(i[9],i[10]),state=i[11],country=NA))
    }
}
fun2009 <- function(i){
    iLen <- length(i)
    if (i[6]=='Van') {
      quickdf(list(time=i[1],oa=i[2],ag=i[3],bib=i[5],lastname=paste(i[6],i[7]),firstname=i[8],gender=i[9],age=i[10],city=paste(i[11],i[12]),state=i[13],country=NA))
    } else if (i[6]=='Kondur') {
      quickdf(list(time=i[1],oa=i[2],ag=i[3],bib=i[5],lastname=paste(i[6],i[7]),firstname=paste(i[8],i[9]),gender=i[10],age=i[11],city=i[12],state=i[13],country=NA))
    } else if (iLen==11) {
      quickdf(list(time=i[1],oa=i[2],ag=i[3],bib=i[5],lastname=i[6],firstname=i[7],gender=i[8],age=i[9],city=i[10],state=i[11],country=NA))
    } else if (iLen==12) {
      quickdf(list(time=i[1],oa=i[2],ag=i[3],bib=i[5],lastname=i[6],firstname=i[7],gender=i[8],age=i[9],city=paste(i[10],i[11]),state=i[12],country=NA))
    } else if (iLen==8) {
      quickdf(list(time=NA,oa=NA,ag=NA,bib=i[2],lastname=i[3],firstname=i[4],gender=i[5],age=i[6],city=i[7],state=i[8],country=NA))
    }
}
fun2010 <- function(i){
    iLen <- length(i)
    if (iLen > 9 && i[10] == 'Red' && i[11] == 'Boiling'){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],bib=i[5],lastname=i[6],firstname=i[7],gender=i[8],age=i[9],city='Red Boiling Springs',state='TN',country=NA))
    } else if (i[6] %in% c('Sims','Anderson')) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],bib=i[5],lastname=paste(i[6],i[7]),firstname=i[8],gender=i[9],age=i[10],city=i[11],state=i[12],country=NA))
    } else if (i[6]=='Clarkson') {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],bib=i[5],lastname=i[6],firstname='J.',gender=i[9],age=i[10],city=i[11],state=i[12],country=NA))
    } else if (iLen==11) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],bib=i[5],lastname=i[6],firstname=i[7],gender=i[8],age=i[9],city=i[10],state=i[11],country=NA))
    } else if (iLen==12) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],bib=i[5],lastname=i[6],firstname=i[7],gender=i[8],age=i[9],city=paste(i[10],i[11]),state=i[12],country=NA))
    } else if (iLen==6) {
      quickdf(list(time=i[1],oa=i[2],ag=NA,bib=NA,lastname='Runner',firstname='Unknown',gender=NA,age=NA,city=NA,state=NA,country=NA))
    } else if (iLen==8) {
      quickdf(list(time=NA,oa=NA,ag=NA,bib=i[2],lastname=i[3],firstname=i[4],gender=i[5],age=i[6],city=i[7],state=i[8],country=NA))
    }
}

fun2011 <- function(i){
    iLen <- length(i)
    if (iLen > 8 && i[9] == 'Red' && i[10] == 'Boiling'){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],gender=i[7],age=i[8],city='Red Boiling Springs',state='TN',bib=i[12],country=NA))
    } else if (i[5] %in% c('Creel','Hancock')){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=paste(i[6],i[7]),gender=i[8],age=i[9],city=i[10],state=i[11],bib=i[12],country=NA))
    } else if (i[5] %in% c('Van','Sims')){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=paste(i[5],i[6]),firstname=i[7],gender=i[8],age=i[9],city=i[10],state=i[11],bib=i[12],country=NA))
    } else if (i[6]=='ChristopheM') {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname='Christopher',gender='M',age=i[7],city=i[8],state=i[9],bib=i[10],country=NA))
    } else if (i[6]=='ShashidharM') {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname='Shashidar',gender='M',age=i[7],city=i[8],state=i[9],bib=i[10],country=NA))
    } else if (i[5]=='UrtuzuasteguiLara'){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname='Urtuzuastegui',firstname='Lara',gender=i[6],age=i[7],city=i[8],state=i[9],bib=i[10],country=NA))
    } else if (iLen==11) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],gender=i[7],age=i[8],city=i[9],state=i[10],bib=i[11],country=NA))
    } else if (iLen==12) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],gender=i[7],age=i[8],city=paste(i[9],i[10]),state=i[11],bib=i[12],country=NA))
    } else if (iLen==13) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],gender=i[7],age=i[8],city=paste(i[9],i[10],i[11]),state=i[12],bib=i[13],country=NA))
    } else if (iLen==8) {
      quickdf(list(time=NA,oa=NA,ag=NA,lastname=i[2],firstname=i[3],gender=i[4],age=i[5],city=i[6],state=i[7],bib=i[8],country=NA))
    }
}

fun2012 <- function(i){
    iLen <- length(i)
    if (i[5]=='Johnson' && i[6] %in% c('Cathie','Troy')){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],bib=i[7],gender=i[8],age=i[9],city='Red Boiling Springs',state='TN',mkills=i[13],country=NA))
    } else if ((i[5]=='Sims' && i[6]=='Jr') || (i[5]=='Van' && i[6]=='Duzee')){ 
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=paste(i[5],i[6]),firstname=i[7],bib=i[8],gender=i[9],age=i[10],city=i[11],state=i[12],mkills=i[13],country=NA))
    } else if (i[5]=='Earles' && i[6]=='Carol'){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],bib=i[7],gender=i[8],age=i[9],city='Ravenden Springs',state='AR',mkills="2",country=NA))
    } else if (i[5]=='Hrynowski'){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=paste(i[6],i[7]),bib=i[8],gender=i[9],age=i[10],city=i[11],state=i[12],mkills=i[13],country=NA))
    } else if (i[5]=='Ho' && i[6] %in% c('Chris','Tina')){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],bib=i[7],gender=i[8],age=i[9],city='North Little Rock',state='AR',mkills=i[13],country=NA))
    } else if (iLen==13){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],bib=i[7],gender=i[8],age=i[9],city=paste(i[10],i[11]),state=i[12],mkills=i[13],country=NA))
    } else if (iLen==14){
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=paste(i[5],i[6]),firstname=paste(i[7],i[8]),bib=i[9],gender=i[10],age=i[11],city=i[12],state=i[13],mkills=i[14],country=NA))
    } else if (iLen==12) {
      quickdf(list(time=i[1],oa=i[2],ag=i[4],lastname=i[5],firstname=i[6],bib=i[7],gender=i[8],age=i[9],city=i[10],state=i[11],mkills=i[12],country=NA))
    } else if (iLen==10){
      quickdf(list(time=NA,oa=NA,ag=NA,lastname=i[2],firstname=i[3],bib=i[4],gender=i[5],age=i[6],city=paste(i[7],i[8]),state=i[9],mkills=i[10],country=NA))
    } else if (iLen==9){
      quickdf(list(time=NA,oa=NA,ag=NA,lastname=i[2],firstname=i[3],bib=i[4],gender=i[5],age=i[6],city=i[7],state=i[8],mkills=i[9],country=NA))
    }
}
parseData <- function(data){
  colNames <- c('lastname','firstname','gender','age','city','state','country','year','bib','time','oa','ag','mkills')
  # 2006 
  x <- subset(data,year==2006)$rLines[4:108]
  dat <-ldply(strsplit(x,'\\s+',perl=TRUE),fun2006)
  dat$year=2006
  dat$mkills=NA
  ret <- dat[,colNames]

  # 2007
  x <- subset(data,year==2007)$rLines[4:177]
  dat <- ldply(strsplit(x,'\\s+',perl=TRUE),fun2007)
  dat$year=2007
  dat$mkills=NA
  ret <- rbind(ret,dat[,colNames])

  # 2008
  x <- subset(data,year==2008)$rLines[4:200]
  dat <- ldply(strsplit(x,'\\s+',perl=TRUE),fun2008)
  dat$year=2008
  dat$mkills=NA
  ret <- rbind(ret,dat[,colNames])

  # 2009
  x <- subset(data,year==2009)$rLines[4:210]
  dat <- ldply(strsplit(x,'\\s+',perl=TRUE),fun2009)
  dat[which(dat$state=='NLD'),'country'] = 'NLD'
  dat[which(dat$country=='NLD'),'state'] = NA
  dat$year=2009
  dat$mkills=NA
  ret <- rbind(ret,dat[,colNames])

  # 2010
  x <- subset(data,year==2010)$rLines[4:238]
  dat <- ldply(strsplit(x,'\\s+',perl=TRUE),fun2010)
  dat[which(dat$state=='GBR'),'country'] = 'GBR'
  dat[which(dat$country=='GBR'),'state'] = NA
  dat[which(dat$state=='NLD'),'country'] = 'NLD'
  dat[which(dat$country=='NLD'),'state'] = NA
  dat$year=2010
  dat$mkills=NA
  ret <- rbind(ret,dat[,colNames])

  # 2011
  x <- subset(data,year==2011)$rLines[4:280]
  dat <- ldply(strsplit(x,'\\s+',perl=TRUE),fun2011)
  dat$year=2011
  dat$mkills=NA
  ret <- rbind(ret,dat[,colNames])

  # 2012
  x <- subset(data,year==2012)$rLines[c(5:285,291:293)]
  dat <- ldply(strsplit(x,'\\s+',perl=TRUE),fun2012)
  dat$year=2012
  ret <- rbind(ret,dat[,colNames])

  # Overall cleaning
  ret$time <- sub('\\*','',ret$time)
  ret[ret$lastname=='Albert'&ret$firstname=='marla','firstname'] <- 'Marla'
  ret[ret$firstname=='A.J','firstname'] <- 'A.J.'
  ret[ret$lastname=='Allison'&ret$firstname=='Matt','firstname'] <- 'Matthew'
  ret[ret$lastname=='Bathula'&ret$firstname=='Shashidar','firstname'] <- 'Shashidhar'
  ret[ret$lastname=='Boyd'&ret$firstname=='Joe','firstname'] <- 'Joseph'
  ret[ret$lastname=='Branham'&ret$firstname=='Tony','lastname'] <- 'Branam'
  ret[ret$lastname=='Bucci'&ret$firstname=='Robert','firstname'] <- 'Rob'
  ret[(ret$lastname %in% c('Schneider','Chappell'))&ret$firstname=='Candice','lastname'] <- 'Chappell-Schneider'
  ret[ret$lastname=='Colee'&ret$firstname=='Andy','firstname'] <- 'Andrew'
  ret[ret$lastname=='Edmonds'&ret$firstname=='Jeffrey','firstname'] <- 'Jeff'
  ret[ret$lastname=='Estes'&ret$firstname=='Christopher','firstname'] <- 'Chris'
  ret[ret$lastname=='Fedor'&ret$firstname=='Elizabeth','firstname'] <- 'Liz'
  ret[ret$lastname=='Girouard'&ret$firstname=='Mike','firstname'] <- 'Michael'
  ret[ret$lastname=='Herrera'&ret$firstname=='Sal','firstname'] <- 'Salome'
  ret[ret$lastname=='Johnson'&ret$firstname=='Russ','firstname'] <- 'Russell'
  ret[ret$lastname=='Kowalski'&ret$firstname=='Kurt','firstname'] <- 'Kurtis'
  ret[ret$lastname=='Lockhardt','lastname'] <- 'Lockhart'
  ret[ret$lastname=='Manning'&ret$firstname=='MonkeyMatt','firstname'] <- 'Matt'
  ret[ret$lastname=='Robinson'&ret$firstname=='Deb','firstname'] <- 'Debra'
  ret[ret$lastname=='Rosenbloom'&ret$firstname=='S. Trent','firstname'] <- 'Trent'
  ret[ret$lastname=='Schaaf','firstname'] <- 'Matthew'
  ret[ret$lastname=='Schneider'&ret$firstname=='Benjamin','firstname'] <- 'Ben'
  ret[ret$lastname%in%c('Teixeira','Teixera'),'lastname'] <- 'Teixera'
  ret[ret$lastname=='Williams'&ret$firstname=='thomas','firstname'] <- 'Tom'
  ret[ret$lastname=='Williams'&ret$firstname=='carole','firstname'] <- 'Carole'
  ret[ret$lastname=='Kezar'&ret$firstname%in%c('edwin','Ed'),'firstname'] <- 'Edwin'
  ret[ret$lastname=='Haugh'&ret$firstname=='Conner','firstname'] <- 'Connor'
  ret[ret$lastname=='Venable'&ret$firstname=='jeff','firstname'] <- 'Jeff'
  ret[ret$lastname=='Bilbrey'&ret$firstname=='Billy','firstname'] <- 'William'
  ret[ret$lastname%in%c('Wagner','Devreese')&ret$firstname=='Rhonda','lastname'] <- 'Wagner-Devreese'
  ret[ret$lastname%in%c('Tietjan','Tietjen')&ret$firstname=='John','lastname'] <- 'Tietjen'
  ret[ret$lastname=='King'&ret$firstname%in%c('randy','RC'),'firstname'] <- 'Randy'
  ret[ret$lastname=='Mcdonald'&ret$firstname=='Debbie','lastname'] <- 'McDonald'
  ret[ret$lastname=='Mckee'&ret$firstname=='Mitchell','lastname'] <- 'McKee'
  ret[ret$lastname=='Presley'&ret$firstname=='trey','firstname'] <- 'Trey'
  ret[ret$lastname=='Harris'&ret$firstname=='Andrew','firstname'] <- 'Drew'
  ret[ret$lastname=='Scott'&ret$firstname=='Scott','lastname'] <- 'Jordan'
  ret[ret$lastname=='Jordan'&ret$firstname=='Scott','firstname'] <- 'Jeffrey'
  ret[ret$lastname%in%c('Cofer','Bucci')&ret$firstname=='Kim','lastname'] <- 'Cofer-Bucci'
  ret[ret$lastname=='Vance'&ret$firstname=='Poss','lastname'] <- 'Poss'
  ret[ret$lastname=='Poss'&ret$firstname=='Poss','firstname'] <- 'Vance'
  ret[ret$lastname=='Runyon'&ret$firstname=='tim','firstname'] <- 'Tim'
  ret[ret$lastname%in%c('Persinger','Reynolds')&ret$firstname=='Dee','lastname'] <- 'Persinger-Reynolds'
  ret[ret$lastname=='Wade'&ret$firstname=='William','firstname'] <- 'Bill'
  ret[ret$lastname%in%c('McGrath','Lawrence')&ret$firstname=='Colleen','lastname'] <- 'McGrath-Lawrence'


  ret$id <- paste(ret$lastname,ret$firstname)

  ret[ret$lastname=='Smith'&ret$firstname=='Dallas'&ret$bib=='824'&ret$year=='2010','id'] <- 'smith dallas-1'

  colNames <- c('lastname','firstname','gender','age','city','state','country','year','bib','time','oa','ag','mkills')
  ret$age <- as.integer(ret$age)
  ret$formattime <- ret$time
  ret$time <- str2sec(ret$time)
  ret$time_hour <- ret$time / 3600
  ret$place <- as.integer(ret$oa)
  ret$ag <- as.integer(ret$ag)
  ret$mkills <- as.integer(ret$mkills)
  ret$gender <- factor(ret$gender,levels=c("M","F"),labels=c("Men","Women"))

  # Monkey Kills defines the number of times a runner has finished the race
  # 
  # As of 2012, Matt Manning's mkill is 5, but I only have 4 of his finishes.
  # and Greg Wright's mkill is 3, but I have 2.
  # John Gurley's mkill is 2, but I hav 1.

  ret$gender_place <- 0L
  for (year in 2006:2012){
    for (gender in c('Men','Women')){
      ids <- which(ret$year==year&ret$gender==gender)
      nas <- which(is.na(ret[ids,'oa']))
      if (length(nas)) {
        ret[ids[1:nas[1]],'gender_place'] <- 1:nas[1]
      } else {
        ret[ids,'gender_place'] <- 1:length(ids)
      }
    }
  }
  for (i in seq(20,75,by=5)){
    if(i == 20){
      label <- "<20"
      ret[which(ret$age < 20),'agegroup'] <- label
    } else if (i == 75){
      label <- ">=75"
      ret[which(ret$age >= 75),'agegroup'] <- label
    } else {
      label <- paste(i,'-',i+4,sep='')
      ret[which(ret$age >= i & ret$age <=i+4),'agegroup'] <- label
    }
  }

  ret
}

getData <- function(rUrl='http://www.harpethhillsmarathon.com/Runners/finishers%d.html'){
  parseData(adply(2006:2012,1,.fun=function(year){
    x <- htmlParse(sprintf(rUrl,year))
    rText <- xmlValue(getNodeSet(x,'//pre')[[1]])
    rLines <- strsplit(rText,'\\r?\\n')[[1]]
    data.frame(rLines=rLines,year=year,stringsAsFactors=FALSE)
  }))
}

repeatOffenders <- function(results){
  ret <- data.frame(id=NULL,lastname=NULL,firstname=NULL,gender=NULL,year=NULL,time=NULL,timeDelta=NULL,offense=NULL,gender_place=NULL)
  for (i in unique(sort(results$id))){
      finishes <- subset(results,id==i,select=c('id','lastname','firstname','gender','year','time','gender_place'))
      if (nrow(finishes)>1){
        finishes$timeDelta <- c(0,diff(finishes$time))
        finishes$offense <- 1:nrow(finishes)
        finishes$gender_place <- max(finishes$gender_place)
        ret <- rbind(ret,finishes)
      }
  }
  ret
}
