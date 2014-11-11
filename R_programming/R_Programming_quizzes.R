setwd("~/Projects/LaddeR/R/")

# Quiz week1
hw1 <- read.csv('hw1_data.csv')
hw1[1:2,]
dim(hw1)
hw1$Ozone[47]
table(is.na(hw1$Ozone))
mean(!is.na(hw1$Ozone)) # mean of logical
mean(hw1$Ozone, na.rm=TRUE)
mean(hw1$Solar.R[!is.na(hw1$Ozone)>31][!is.na(hw1$Temp)>90], na.rm=TRUE)
mean(hw1$Temp[hw1$Month==6], na.rm=TRUE)
max(hw1$Ozone[hw1$Month==5], na.rm=TRUE)
