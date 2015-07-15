library(tree)
library(ISLR)

attach(Carseats)

High = ifelse(Sales <= 8, "No", "Yes")

Carseats = data.frame(Carseats, High)

tree.carseats = tree(High~.-Sales, Carseats)

summary(tree.carseats)


windows()
plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats

set.seed(2)
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
