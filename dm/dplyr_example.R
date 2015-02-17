library(dplyr)
library(ggplot2)
library(datasets)

data(package = .packages(all.available=TRUE))
data(diamonds)
head(diamonds)

outcome <- list('price')
outcome[1]
for (var in outcome) {
  print(var)
  test <- qplot(carat, diamonds[,var], data=diamonds)
  test
  summary(test)
}
test
print(test)
outcome <- "price"
test <- qplot(carat, outcome, data=diamonds)
summary(test)

qplot(carat, price, data=diamonds)
qplot(log(carat), log(price), data=diamonds)
qplot(carat, price/carat, data=diamonds)

qplot(carat, price, data=diamonds, colour=color)
qplot(carat, price, data=diamonds, size=carat, colour=color)
qplot(carat, price, data=diamonds, size=carat, colour=color, shape=cut)
qplot(price, carat, data=diamonds, facets=. ~ color)
qplot(price, carat, data=diamonds, facets=color ~ clarity)

qplot(cut, data=diamonds, geom="bar")
qplot(price, data=diamonds, geom="histogram")
qplot(price, data=diamonds, geom="histogram", color=color)
qplot(price, data=diamonds, geom="histogram", fill=color)
qplot(price, data=diamonds, geom="histogram", binwidth=100) # # of bins
qplot(price, data=diamonds, geom="histogram", xlim=c(0, 5000)) # zooming

qplot(carat, data=diamonds, geom="density", colour=color)
qplot(carat, data=diamonds, geom="histogram", fill=color, binwidth=0.5)

qplot(color, price/carat, data=diamonds, geom="boxplot", colour=color)
qplot(color, price/carat, data=diamonds, geom="boxplot", fill=color)

qplot(color, price / carat, data=diamonds, geom="jitter", colour=color, alpha=I(1/8))


df.diamonds_ideal <- filter(diamonds, cut=="Ideal") # select rows
df.diamonds_ideal <- select(df.diamonds_ideal, carat, cut, color, price, clarity) # select vars
df.diamonds_ideal <- mutate(df.diamonds_ideal, price_per_carat = price/carat) # create new var
df.diamonds_ideal <- arrange(df.diamonds_ideal, color) # order, but doesn't take multiple var
df.diamonds_ideal <- arrange(df.diamonds_ideal, desc(price))
summarize(df.diamonds_ideal, avg_price=mean(price, na.rm=TRUE))

# chaining in dplyr
df.diamonds_ideal_chained <- diamonds %>%
  filter(cut=="Ideal") %>%
  select(carat, cut, color, price, clarity) %>%
  mutate(price_per_carat = price/carat)

diamonds %>%
  filter(cut=="Ideal") %>%
  ggplot(aes(x=color, y=price)) + geom_boxplot()

diamonds %>%
  filter(cut=="Ideal") %>%
  ggplot(aes(price)) + geom_histogram() + facet_wrap(~ color)