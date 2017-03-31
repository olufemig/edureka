library(dplyr)
library(tidyr)
library(ggplot2)
df.retail <- read.csv("Retail_Case_Study_Data.csv")
summary(df.retail)
#there are no nulls
str(df.retail)
table(df.retail$Purchase.Channel)
#tail(df.retail)
# mens and women merchandise, new customer and visited website and sale made are factors
# and needs to be converted
#df.retail$Mens.Merchandise <- as.factor(df.retail$Mens.Merchandise)
#df.retail$Womens.Merchandise <- as.factor(df.retail$Womens.Merchandise)
#df.retail$New.Customer <- as.factor(df.retail$New.Customer)
#df.retail$Visited.Website <- as.factor(df.retail$Visited.Website)
#df.retail$Sale.Made <- as.factor(df.retail$Sale.Made)
#dependent variable needs to be integer
#df.retail$Sale.Made<- as.numeric(as.character(df.retail$Sale.Made))
#take out the customer id and spend category
df.retail = subset(df.retail, select = -c(1,3))
str(df.retail)
# split input and output 
x <- df.retail[,1:8] 
y <- df.retail[,9]
#boxplot for all dependent variables
#par(mfrow=c(1,8)) 
#for(i in 1:8) {
#boxplot(x[,i], main=names(df.retail)[i]) }


