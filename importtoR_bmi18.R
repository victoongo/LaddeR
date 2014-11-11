library("foreign")
mydata<-read.spss("D:/E_Risk/BMIdata/age18/Candice_23Jan14.sav")

names(mydata)
var.labels <- attr(mydata,"variable.labels")
var.labels
attach(mydata)
summary(mydata)

data.key <- data.frame(mydata)
data.key
write.table(data.key, "D:/E_Risk/BMIdata/age18/Candice_23Jan14.txt", sep="\t")
