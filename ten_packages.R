 
# 2 forecast
library(forecast)

# mdeath: Monthly Deaths from Lung Diseases in the UK
fit <- auto.arima(mdeaths)
# customize your confidence intervals
forecast(fit, level=c(80, 95, 99), h=3)
plot(forecast(fit), shadecols="oldstyle")


# 3 plyr
library(plyr)
# split a data frame by Species, summarize it, then convert the results into a data frame
ddply(iris, .(Species), summarise, mean_petal_length=mean(Petal.Length))

# split a data frame by Species, summarize it, then convert the results into an array
unlist(daply(iris[,4:5], .(Species), colwise(mean)))


# 8 qcc
library(qcc)
# series of values w/ mean of 10 with a little random noise added in 
x <- rep(10, 100) + rnorm(100)
# a test series wi/ a mean of 11
new.x <- rep(11, 15) + rnorm(15)
# qcc will flag the new points
qcc(x, newdata=new.x, type="xbar.one")

# 9 reshape2
library(reshape2)
# generate a unique id for each row: this let's us go back to wide format later
iris$id <- 1:nrow(iris)

iris.lng <- melt(iris, id=c("id", "Species"))
head(iris.lng)
iris.wide <- dcast(iris.lng, id + Species ~ variable)
head(iris.wide)

library(ggplot2)
# plots a histogram for each numeric column in the dataset
p <- ggplot(aes(x=value, fill=Species), data=iris.lng)
p + geom_histogram() + 
  facet_wrap(~variable, scales="free")


# 10 randomForest
library(randomForest)

# download Titanic Survivors data
data <- read.table("http://math.ucdenver.edu/RTutorial/titanic.txt", h=T, sep="\t")
# make survived into a yes/no
data$Survived <- as.factor(ifelse(data$Survived==1, "yes", "no"))

# split into a training and test set
idx <- runif(nrow(data)) <= .75
data.train <- data[idx,]
data.test <- data[-idx,]

# train a random forest
rf <- randomForest(Survived ~ PClass + Age + Sex, 
    data=data.train, Importance=TRUE, na.action=na.omit)

# how important is each variable in the model
imp <- importance(rf)
o <- order(imp[.3], decreasing=T)
imp[0,]

# confusion matrix [[True Neg, False Pos], [False Neg, True, Pos]]
table(data.test$Survived, predict(rf, data.test), dnn=list("actual", "predicted"))
