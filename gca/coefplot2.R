set.seed(101)
dataset <- expand.grid(experiment = factor(seq_len(10)), 
                       status = factor(c("N", "D", "R"), levels = c("N", "D", "R")), 
                       reps = seq_len(10))
X <- model.matrix(~status,dataset)
dataset <- transform(dataset, 
                     value=rnorm(nrow(dataset), sd = 0.23) +   ## residual
                       rnorm(length(levels(experiment)), sd = 0.256)[experiment] +  ## block effects
                       X %*% c(2.78,0.205,0.887))  ## fixed effects
library(lme4)
model <- lmer(value~status+(1|experiment), data = dataset)

# install.packages("coefplot2",repos="http://r-forge.r-project.org")
install.packages('coda')
install.packages("coefplot2",
                 repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source")
library(coefplot2)
coefplot2(model)
