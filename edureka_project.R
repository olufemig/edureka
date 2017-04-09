library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(caret)
df.retail <- read.csv("Retail_Case_Study_Data.csv")
head(df.retail)
#there are no nulls
summary(df.retail)
table(df.retail$Purchase.Channel)
#take out the customer id and spend category
df.retail = subset(df.retail, select = -c(1,3))
# split input and output 
#x <- df.retail[,1:4] 
#y <- df.retail[,9]
pairs(df.retail[,-c(5,7)])
# very little correlation between the numeric variables
str(df.retail)
barplot(xtabs(~df.retail$Area))
# most users are suburban which means they maybe based where the stores are located
ggplot(df.retail, aes(x=Spend.Numeric)) +
  geom_histogram(binwidth = 150)
boxplot(df.retail$Spend.Numeric, horizontal = TRUE)
barplot(table(df.retail$Area))
plot(Spend.Numeric~Purchase.Channel, data=df.retail)
pairs(df.retail[,c(1,2,5,6,7)])
table(df.retail$Area)
plot(df.retail$Area ~ df.retail$Spend.Numeric)
# next is data modelling