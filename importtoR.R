library("foreign")
mydata<-read.spss("D:/E_Risk/scii/SCII-Complete_cases_Sept2008.sav")
names(mydata)
var.labels <- attr(mydata,"variable.labels")
var.labels
data.key <- data.frame(var.name=names(mydata),var.labels)
data.key <- data.frame(var.labels)
data.key
write.table(data.key, "D:/E_Risk/scii/scii_lables.txt", sep="\t")