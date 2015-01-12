# doParallelDO work in Windows!

library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
foreach(i=1:3) %dopar% {
  d <- sqrt(i)
  d
}

names(iris)
y_var <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
for (y in y_var) {y <- 1}
# ptime <- system.time({
# r <- foreach(y=y_var, .verbose=TRUE, .combine=rbind) %dopar% {
#     foreach(y=y_var, .verbose=TRUE, .combine=rbind) %dopar% {
      for(y in y_var) {
#     fmla <- paste(y, "~factor(Species)")
#     result <- data.frame(coef(summary(lm(as.formula(fmla), data=iris))))
#     fmla <- as.formula(paste(y, "~factor(Species)"))
#     fmla <- paste(y, "~factor(Species)")
#     result <- data.frame(coef(summary(lm(fmla, data=iris))))
#     result$outcome <- y
#     return(result)
    fmla <- paste(y, "~factor(Species)")
#     out <- paste(y)
#     y <- data.frame(coef(summary(lm(fmla, data=iris))))
    y[] <- data.frame(coef(summary(lm(fmla, data=iris))))
  }
# })[3]
ptime

tb <- data.frame(coef(summary(result)))
# summary(lm(Sepal.Length~factor(Species), data=iris))

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000

ptime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
ptime

stime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %do% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
stime