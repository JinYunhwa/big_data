install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
library(dplyr)
library(tidyverse)

a <- read.csv("groceries.csv", header=FALSE)
View(a)

groceris <- read.transactions("groceries.csv", sep = ",")

summary(groceris)

inspect(groceris[1:5])

itemFrequency(groceris[,1:3])

itemFrequencyPlot(groceris, supprt=0.1)

itemFrequencyPlot(groceris, topN=20)

myrules <- apriori(data=groceris, parameter = list(support=0.01, confidence=0.25,minlen=2))
myrules0 <- apriori(data=groceris, parameter = list(support=0.006, confidence=0.25,minlen=2))

summary(myrules)

inspect(myrules[1:3])

#연관규칙 정렬
inspect(sort(myrules, by="lift")[1:10])
inspect(sort(myrules0, by="lift")[1:10])

#연관규칙 부분 집합
berryrule <- subset(myrules, items %in% "berries")

inspect(berryrule)

myrules2 <- sort()

groceryrule <- as(myrules, "data.frame")

