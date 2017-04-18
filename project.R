library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(caret)
df.retail <- read.csv("Retail_Case_Study_Data.csv")
str(df.retail)
table(df.retail$Purchase.Channel)
#take out the customer id 
df.retail = df.retail[,-1]
#convert spend category and dependent variable to factor
df.retail$Spend.Category<-as.factor(df.retail$Spend.Category)
df.retail$Sale.Made<-as.factor(df.retail$Sale.Made)
str(df.retail)
#see if any correlation between numeric variables
pairs(df.retail[,-c(2,6,8)])
# univariate analysis
barplot(xtabs(~df.retail$Area))
# most users are suburban which means they maybe based where the stores are located
ggplot(df.retail, aes(x=Spend.Numeric)) +
  geom_histogram(binwidth = 150)
boxplot(df.retail$Spend.Numeric, horizontal = TRUE)
#side by side boxplot
boxplot(df.retail$Spend.Numeric~ df.retail$Area)
validationIndex <- createDataPartition(df.retail$Sale.Made, p=0.80, list=FALSE) 
# select 20% of the data for training 
validation <- df.retail[-validationIndex,] 
# use the remaining 80% of data for training and testing the models 
dataset <- df.retail[validationIndex,]
str(dataset)
classifier= glm(formula=Sale.Made~.,
                family=binomial,
                data=dataset)
summary(classifier)
classifier= glm(formula=Sale.Made~Months.Since.Last.Buy,Spend.Category,New.Customer,
                family=binomial,
                data=dataset)
prob_pred=predict(classifier,type='response',newdata=validation[-10])
y_pred=ifelse(prob_pred>0.5,1,0)
cm=table(validation[,10],y_pred)
summary(cm)
