pollutantmean <- function(directory, pollutant, id = 1:332) {
#   df <- data.frame(cbind(id))
  corrv <- c()
  for (i in 1:length(id)) {
    i3 <- sprintf("%03.f", id[i])
    fn <- paste(directory,"/",i3,".csv", sep="")
    fn
    dat <- read.csv(fn)
    dat
    corri <- subset(dat,select=c(pollutant))
    corri <- corri[,1]
    corrv <- c(corrv, corri)
  }
  corrv
  tmp <- mean(corrv, na.rm=T)
  return(tmp)
}
# debug(pollutantmean)
# pollutantmean("specdata", "sulfate", 1:10)
# 
# directory<- 'specdata'
# pollutant<-'sulfate
# id<-1:20



