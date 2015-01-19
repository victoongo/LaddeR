corr <- function(directory, threshold = 0) {
  df <- complete("specdata")
  sub <- df[df$nobs>threshold,][,1]
  corrv = c()
  
  if (length(sub)>0) {
    for (i in 1:length(sub)) {
      i3 <- sprintf("%03.f", sub[i])
      fn <- paste(directory,"/",i3,".csv", sep="")
  #     print(fn)
      dat <- read.csv(fn)
  #     dat
      corri<-cor(dat$sulfate, dat$nitrate, use="pairwise")
  #     corri
      corrv <- c(corrv, corri)
      
    }
  } 
  return(corrv)
}
# debug(corr)
#  corr("specdata", 2000)
