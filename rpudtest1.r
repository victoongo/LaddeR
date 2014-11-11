x <- mtcars["Honda Civic",] 
y <- mtcars["Camaro Z28",] 
dist(rbind(x, y))
z <- mtcars["Pontiac Firebird",] 
dist(rbind(y, z))

dist(as.matrix(mtcars))

test.data <- function(dim, num, seed=17) { 
  set.seed(seed) 
  matrix(rnorm(dim * num), nrow=num) 
} 
m <- test.data(120, 4500) 
system.time(dist(m)) 

#library(rpud)                  # load rpud with rpudplus 
#system.time(rpuDist(m)) 