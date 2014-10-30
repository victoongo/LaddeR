library("foreign")
library(ggplot2)
setwd("D:/E_Risk/crime")
crimes2011 <- read.dta("D:/E_Risk/crime/crimes2011.dta")

attach(crimes2011)
summary(crimes2011)

png("r_plot.png", width = 420, height = 340)
hist(ttlcrm2011, breaks=30, main=paste("Car Distribution"), xlab="Number of Gears")
dev.off()
qplot(ttlcrm2011,data=crimes2011)

clist <- c("ttlcrm2011", "shopli")
for (i in clist) {
	png("i.png", width = 420, height = 340)
	hist(get(i), breaks=25, main=paste("Histogram of", i), xlab=i)
	dev.off()
}

clist <- c("ttlcrm2011", "shopli")
for (i in clist) {
	hist(get(i), breaks=25, main=paste("Histogram of", i), xlab=i)
	savePlot("i.png", type = "png")
}

clist <- c("ttlcrm2011", "shopli")
for (i in "ttlcrm2011" "shopli") {
	png(i, width = 420, height = 340)
	hist(get(i), main="Car Distribution", xlab="Number of Gears")
}

*****
library(R2HTML)
library(ggplot2)
HTMLStart(outdir="D:/E_Risk/crime/", file="crimeall",
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

