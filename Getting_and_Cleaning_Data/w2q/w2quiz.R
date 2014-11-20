
## Q1
library(httr)
library(jsonlite)
library(data.table)
library(sqldf)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
myapp <- oauth_app("github", "9fd7494d28cdc9900c0c", "f4380340a22aad674e895911d9c5e08cdf2df76f")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
json1 = content(req)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[which(json2$name=="datasharing"),"created_at"]

# OR:
req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
stop_for_status(req)
content(req)

## Q2
setwd("~/Projects/LaddeR/Getting_and_Cleaning_Data/w2q/")
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile="w2.csv", method="curl")
acs <- fread("w2.csv")


dat <- sqldf("select pwgtp1 from acs where AGEP<50")
table(dat)
sqldf("select * from acs where AGEP < 50 and pwgtp1")
sqldf("select * from acs")
sqldf("select * from acs where AGEP < 50")

## Q3 
unique(acs$AGEP)
sqldf("select unique AGEP from acs")
sqldf("select distinct AGEP from acs") # this one
sqldf("select AGEP where unique from acs")
sqldf("select distinct pwgtp1 from acs")

## Q4
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
l <- c(10, 20, 30,100)
nchar(htmlCode[l])
library(stringi)
stri_length()
library(stringr)
str_length()

## Q5

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(fileURL, destfile="w2_5.for", method="curl")
q5 <- read.fwf("w2_5.for",widths = c(10,9,4,9,4,9,4,9,4),header=FALSE,skip=4,)
sum(q5[4])
