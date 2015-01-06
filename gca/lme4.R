library(ggplot2)
library(lme4)
library(multcomp)
dataset <- expand.grid(experiment = factor(seq_len(10)), status = factor(c("N", "D", "R"), levels = c("N", "D", "R")), reps = seq_len(10))
dataset$value <- rnorm(nrow(dataset), sd = 0.23) + with(dataset, rnorm(length(levels(experiment)), sd = 0.256)[experiment] + ifelse(status == "D", 0.205, ifelse(status == "R", 0.887, 0))) + 2.78
model <- lmer(value~status+(1|experiment), data = dataset)
tmp <- as.data.frame(confint(glht(model, mcp(status = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()

tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()

model <- lmer(value ~ 0 + status + (1|experiment), data = dataset)
tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()






set.seed(42)
x <- rep(0:100,10)
y <- 15 + 2*rnorm(1010,10,4)*x + rnorm(1010,20,100)
id<-rep(1:10,each=101)

dtfr <- data.frame(x=x ,y=y, id=id)

library(nlme)

model.mx <- lme(y~x,random=~1+x|id,data=dtfr)

#create data.frame with new values for predictors
#more than one predictor is possible
new.dat <- data.frame(x=0:100)
#predict response
new.dat$pred <- predict(model.mx, newdata=new.dat,level=0)

#create design matrix
Designmat <- model.matrix(eval(eval(model.mx$call$fixed)[-2]), new.dat[-ncol(new.dat)])

#compute standard error for predictions
predvar <- diag(Designmat %*% model.mx$varFix %*% t(Designmat))
new.dat$SE <- sqrt(predvar) 
new.dat$SE2 <- sqrt(predvar+model.mx$sigma^2)

library(ggplot2) 
p1 <- ggplot(new.dat,aes(x=x,y=pred)) + 
  geom_line() +x
  geom_ribbon(aes(ymin=pred-2*SE2,ymax=pred+2*SE2),alpha=0.2,fill="red") +
  geom_ribbon(aes(ymin=pred-2*SE,ymax=pred+2*SE),alpha=0.2,fill="blue") +
  geom_point(data=dtfr,aes(x=x,y=y)) +
  scale_y_continuous("y")
p1





x<-rep(seq(0,100,by=1),10)
y<-15+2*rnorm(1010,10,4)*x+rnorm(1010,20,100)
id<-NULL
for (i in 1:10){
  id<-c(id, rep(i,101))}
dtfr<-data.frame(x=x,y=y, id=id)
library(nlme)
with (dtfr, summary(lme((y)~x,random=~1+x|id, na.action=na.omit  )))
model.mx<-with (dtfr, (lme((y)~x,random=~1+x|id, na.action=na.omit  )))
pd<-predict(model.mx, newdata=data.frame(x=0:100),level=0)
with (dtfr, plot(x, y))
lines(0:100,predict(model.mx, newdata=data.frame(x=0:100),level=0), col="darkred", lwd=7)