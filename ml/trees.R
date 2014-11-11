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
*****

library(rpart)
library(DMwR)
library(party)
library(GGally)

## Regression Trees ##

# ses P10ACORN5 SESDM5 Tot_Deprived_Area
# hvio harme510 tvi2m10
# nvio ttlcrm2011 s2nprob
# fh fhanypm12 
# dep masce12 totdcdE12 cdie12 bmie11p95 triede_cig12 alcmultiple

reg1 <-lm(bmie11 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area)
summary(reg1)

#####
data_sex1 = subset(bmi_pred, subset = sampsex == 1)
attach(data_sex1)
CART1 <- rpart(bmie11 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART1)
YHAT.CART1 <- predict(CART1)
PRED.CART1 <- cor(YHAT.CART1, bmie11)**2
PRED.CART1

data_sex2 = subset(bmi_pred, subset = sampsex == 2)
attach(data_sex2)
CART2 <- rpart(bmie11 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART2)
YHAT.CART2 <- predict(CART2)
PRED.CART2 <- cor(YHAT.CART2, bmie11)**2
PRED.CART2

#####
data_sex1 = subset(bmi_pred, subset = sampsex == 1)
#data_sex1 = na.omit(data_sex1)
attach(data_sex1)
CART1 <- rpart(totdcdE12 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART1)
YHAT.CART1 <- predict(CART1)
PRED.CART1 <- cor(YHAT.CART1, totdcdE12)**2
PRED.CART1

data_sex2 = subset(bmi_pred, subset = sampsex == 2)
#data_sex2 = na.omit(data_sex2)
attach(data_sex2)
CART2 <- rpart(totdcdE12 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART2)
YHAT.CART2 <- predict(CART2)
PRED.CART2 <- cor(YHAT.CART2, totdcdE12)**2
PRED.CART2

#####
data_sex1 = subset(bmi_pred, subset = sampsex == 1)
#data_sex1 = na.omit(data_sex1)
attach(data_sex1)
CART1 <- rpart(triede_cig12 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART1)
YHAT.CART1 <- predict(CART1)
PRED.CART1 <- cor(YHAT.CART1, triede_cig12)**2
PRED.CART1

data_sex2 = subset(bmi_pred, subset = sampsex == 2)
#data_sex2 = na.omit(data_sex2)
attach(data_sex2)
CART2 <- rpart(triede_cig12 ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART2)
YHAT.CART2 <- predict(CART2)
PRED.CART2 <- cor(YHAT.CART2, triede_cig12)**2
PRED.CART2

#####
data_sex1 = subset(bmi_pred, subset = sampsex == 1)
#data_sex1 = na.omit(data_sex1)
attach(data_sex1)
CART1 <- rpart(alcmultiple ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART1)
YHAT.CART1 <- predict(CART1)
PRED.CART1 <- cor(YHAT.CART1, alcmultiple)**2
PRED.CART1

data_sex2 = subset(bmi_pred, subset = sampsex == 2)
#data_sex2 = na.omit(data_sex2)
attach(data_sex2)
CART2 <- rpart(alcmultiple ~ SCIIurban3 + sesdism5 + P12ACORN5 + P12HealthACORN07 + Tot_Deprived_Area + Dep_Quart + intersect + density + PerGreen + obstruction + blockgreen + numGreen + ttlcrm2011 + PdisorderM_mean + PhysDecay1_mean + s2contr + s2cohe + s2coef + s2coef10 + s2ndsrdr + s2ncrime + s2nprob + s2conect + s2like + s2padult + s2fear + s2safe + s2victim + SCIIB1 + SCIIB2 + SCIIB3 + SCIIB4 + SCIIB5 + SCIIB6 + SCIIB7 + SCIIB8 + SCIIB9 + SCIIB10 + SCIIB11 + SCIIB12 + SCIIB13 + SCIIB14 + SCIIB15 + SCIIB16 + SCIIB17 + SCIIB18 + SCIIB19 + SCIIB20)
prettyTree(CART2)
YHAT.CART2 <- predict(CART2)
PRED.CART2 <- cor(YHAT.CART2, alcmultiple)**2
PRED.CART2




CART1 <- rpart(masce12 ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
prettyTree(CART1)
CART1 <- rpart(totdcdE12 ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
prettyTree(CART1)
CART1 <- rpart(cdie12 ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
prettyTree(CART1)
CART1 <- rpart(bmie11p95 ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
prettyTree(CART1)
CART1 <- rpart(triede_cig12 ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
prettyTree(CART1)
CART1 <- rpart(alcmultiple ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
prettyTree(CART1)


data_subset = subset(bmi_pred, subset = bmie11 != NA & SCIIurban3 != NA & P12ACORN5 != NA & s2contr != NA & s2coef != NA)

data_cols = subset(bmi_pred, select = c('bmie11', 'SCIIurban3', 'P12ACORN5', 's2contr', 's2coef', 'SESDM5', 'Tot_Deprived_Area', 'harme510', 'tvi2m10', 'ttlcrm2011', 's2nprob', 'fhanypm12'))
data_listwise = na.omit(data_cols)


attach(data_listwise )
CART2 <- ctree(bmie11 ~ SCIIurban3 + P12ACORN5 + s2contr + s2coef + SESDM5 + Tot_Deprived_Area + harme510 + tvi2m10 + ttlcrm2011 + s2nprob + fhanypm12)
plot(CART2)
text(CART2)







#text(CART1, use.n=T, cex=.9, fancy=F)
CART1
YHAT.CART1 <- predict(CART1)
plot(YHAT.CART1, pc_bd)
PRED.CART1 <- cor(YHAT.CART1, pc_bd)**2
PRED.CART1
