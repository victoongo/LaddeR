# 3D plots with ggplot2 and plotly

install.packages("devtools") # so we can install from github
library('devtools')
install_github("ropensci/plotly") # plotly is part of ropensci
library(plotly)

py <- plotly(username="r_user_guide", key="mw5isa4yqp") # open plotly connection

pp <- function (n,r=4) {
  x <- seq(-r*pi, r*pi, len=n)
  df <- expand.grid(x=x, y=x)
  df$r <- sqrt(df$x^2 + df$y^2)
  df$z <- cos(df$r^2)*exp(-df$r/6)
  df
}
p <- ggplot(pp(20), aes(x=x,y=y))

p <- p + geom_tile(aes(fill=z))

py$ggplotly(p)

qplot(x, y, data=pp(100), geom="tile", fill=z)

py$ggplotly()



data <- list(
  list(
    z = matrix(c(1,20,30,50,1,20,1,60,80,30,30,60,1,-10,20), nrow=3, ncol=5), 
    x = c("Monday","Tuesday","Wednesday","Thursday","Friday"),
    y = c("Morning","Afternon","Evening"), 
    type = "surface"
  )
)
response <- py$plotly(data, kwargs=list(filename="3D surface", fileopt="overwrite"))
url <- response$url # returns plot url