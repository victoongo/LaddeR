# week 1 quiz
library(data.table)
library(xlsx)
library(RCurl)
library(XML)

setwd("~/Projects/LaddeR/Getting_and_Cleaning_Data/w1q")

# csv
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile="w1d.csv", method="curl")
list.files(".")
dateDownloaded <- date()
dateDownloaded
w1d <- fread("w1d.csv")
w1d
str(w1d)
names(w1d)
table(w1d$VAL==24)
table(w1d$FES)

# xlsx
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileURL, destfile="w1d.xlsx", method="curl")
dateDownloaded <- date()
list.files(".")
colIndex <- 7:15
rowIndex <- 18:23
dat <- read.xlsx("w1d.xlsx",sheetIndex=1,colIndex=colIndex,rowIndex=rowIndex,header=TRUE)
sum(dat$Zip*dat$Ext, na.rm=T)

# xml
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xData <- getURL(fileURL)
doc <- xmlTreeParse(xData, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]][[1]]
rootNode[[1]][[1]][[1]]
xmlSApply(rootNode,xmlValue)
zip <- xpathSApply(rootNode,"//zipcode",xmlValue)
table(zip==21231)

# DT
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile="q5.csv", method="curl")
list.files(".")
dateDownloaded <- date()
dateDownloaded
DT <- fread("q5.csv")

tapply(DT$pwgtp15,DT$SEX,mean)
mean(DT$pwgtp15,by=DT$SEX)
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
sapply(split(DT$pwgtp15,DT$SEX),mean)
DT[,mean(pwgtp15),by=SEX]

system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2])
system.time(mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(DT[,mean(pwgtp15),by=SEX])
