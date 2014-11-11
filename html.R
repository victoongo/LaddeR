bmi_pred <- read.csv("D:/E_Risk/BMIdata/bmi_pred.txt", header = T)

attach(bmi_pred)
summary(bmi_pred)

*****
library(R2HTML)
library(ggplot2)
HTMLStart(outdir="D:/E_Risk/BMIdata/", file="myreport",
   extension="html", echo=FALSE, HTMLframe=TRUE)

HTML.title("My Report", HR=1)
HTML.title("Description of my data", HR=3)
table(early_expE)
HTMLhr()

HTML.title("My Reg", HR=1)
HTML.title("Reg of my data", HR=3)
reg1 <-lm(bmie11 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area)
summary(reg1)
HTMLhr()

HTML.title("X Y Scatter Plot", HR=2)
*counts <- table(ttlcrm2011)
hist(ttlcrm2011, main="Car Distribution",
	xlab="Number of Gears")
*qplot(ttlcrm2011,data=bmi_pred)
/*,facets=geno~.)*/
HTMLplot()

HTMLStop() 