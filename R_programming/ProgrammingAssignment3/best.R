best <- function(state, outcome) {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #   state <- "NY"
  #   outcome <- "pneumonia"
  if (sum(state %in% df$State)>0) {
    df <- filter(df, State==state)
    if (outcome=="pneumonia") {
      df <- df[, c(2, 23)]
    }
    if (outcome=="heart attack") {
      df <- df[, c(2,11)]
    }
    df <- df[!is.na(df[,2]),]
    df[,2] <- as.numeric(df[,2])
    names(df) <- c("h", "d")
    arrange(df, d, h)[1,1]
    #     df[,2] <- as.numeric(df[,2])
    #     df[order(df[,2]),1][1]
  } else {
    stop("invalid state")
  }
}

best("NC", "pneumonia")