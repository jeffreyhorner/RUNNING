library(GeoIP)
library(RAmazonS3)

downloadLogs <- function(){
  logFile <- tempfile(pattern=paste(as.character(Sys.Date()),'.',sep=''),tmpdir='data/logs',fileext='.log')
  x <- listBucket(getOption('RUNNING.AmazonS3.bucket'))
  fileNames <- grep('^logs/',as.character(x$Key),value=TRUE)
  lapply(fileNames,function(i) write(getFile(getOption('RUNNING.AmazonS3.bucket'),i),file=logFile,append=TRUE))
  lapply(fileNames,function(i) removeFile(getOption('RUNNING.AmazonS3.bucket'),i))
  invisible()
}

ipList <- function(logFile='data/logs/2013-11-26.64522e938f9d.log'){
  ips <- 
    system(
      sprintf(
        "grep 'REST.GET.OBJECT fancybox/source/helpers/jquery.fancybox-buttons.css' %s | awk '{print $5}' | sort -u",
        logFile
      ),
      intern=TRUE
    )
  geoip.full(ips)
}

