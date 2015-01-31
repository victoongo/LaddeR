rankhospital <- function(state, outcome, rank) {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#     state <- "NY"
#     outcome <- "pneumonia"
#     rank <- 3
  if (sum(state %in% df$State)>0) {
    df <- filter(df, State==state)
    if (outcome=="pneumonia") {
      df <- df[, c(2, 23)]
    }
    else if (outcome=="heart attack") {
      df <- df[, c(2,11)]
    }
    else {
      stop('invalid outcome')
    }
#     print(df)
    df <- df[!is.na(df[,2]),]
    df[,2] <- as.numeric(df[,2])
    names(df) <- c("h", "d")
#     print(df)
    if (rank=="worst"){
      arrange(df, desc(d), h)[1,1]
    } else if (rank>length(df$d)) {
      NA
    } else {
#       print(df)
      df <- arrange(df, d, h)
#       print(df)
      df[rank,1]
    }
    #     df[,2] <- as.numeric(df[,2])
    #     df[order(df[,2]),1][1]
  } else {
    stop("invalid state")
  }
}

# rankhospital("NC", "heart attak", 7)
