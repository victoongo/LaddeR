rankall <- function(outcome, rank) {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#   outcome <- "pneumonia"
#   rank <- 3
  
  if (outcome=="pneumonia") {
    df <- df[, c(2, 7, 23)]
  } else if (outcome=="heart attack") {
    df <- df[, c(2, 7, 11)]
  } else if (outcome=="heart failure") {
    df <- df[, c(2, 7, 17)]
  } else {
    stop('invalid outcome')
  }
  
  df[,3] <- as.numeric(df[,3])
#   df <- df[!is.na(df[,3]),]
  names(df) <- c("h", "s", "d")
#   byg <- group_by(df, s)
  st <- split(df, df$s)
  if (rank=="worst"){
    sta <- NULL
    for (i in 1:length(st)) {
      stb <- arrange(st[[i]], desc(d), h)[1,c(1,2)]
      sta <- rbind(sta, stb)
    } 
    names(sta) <- c("hospital", "state")
    return(sta)
#     head(sta)
#     apply(st,1,arrange(desc(d), h))
#     c <- arrange(byg, desc(d), h)
#     head(c)
#     wrst <- by(c, c$s,head,n=1)
#     wst <- unlist(wrst)
#     [1,1]
  } else {
    sta <- NULL
    for (i in 1:length(st)) {
      stb <- arrange(st[[i]], d, h)[rank,c(1,2)]
      sta <- rbind(sta, stb)
    } 
#     head(sta)
    names(sta) <- c("hospital", "state")
    return(sta)
  }
}
# 
# out <-  rankall("heart attack", 4)
# tail(rankall("heart failure", 10))
# tail(rankall("pneumonia", "worst"), 3)
