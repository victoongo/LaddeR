complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  df <- data.frame(cbind(id))
  df$nobs <- NA
  for (i in 1:length(id)) {
#     fn <- paste(ifelse(id<10, "00", ifelse(id<100, "0", "")),id,sep="")
    
#     if((id>=1) && (id<10)) {
#       fn <- paste("00",id,sep="")
#       print(fn)
#     } else if((id>=10) && (id<=99)) {
#       fn <- paste("0",id,sep="")
#       print(fn)
#     } else {
#       fn <- id
#       print(fn)
#     }
    
    i3 <- sprintf("%03.f", id[i])
#     print(i3)
    fn <- paste(directory,"/",i3,".csv", sep="")
    dat <- read.csv(fn)
    df$nobs[i] <- sum(complete.cases(dat))
    
  }
  return(df)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}
# complete("specdata", c(6, 10, 20, 34, 100, 200, 310))