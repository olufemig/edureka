library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(caret)
df.retail <- read.csv("Retail_Case_Study_Data.csv")
df.retail$Sale.Made<- as.factor(df.retail$Sale.Made)
#head(df.retail)
#there are no nulls
str(df.retail)
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
#split the data into training and test
# create a list of 80% of the rows in the original dataset we can use for training 
validationIndex <- createDataPartition(df.retail$Sale.Made, p=0.80, list=FALSE) 
# select 20% of the data for validation 
validation <- df.retail[-validationIndex,] 
# use the remaining 80% of data to training and testing the models 
df.mydata <- df.retail[validationIndex,]
str(df.mydata)
#create validation harness
trainControl <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# LDA 
set.seed(7) 
fit.lda <- train(Sale.Made~., data=df.mydata, method="lda", metric=metric, trControl=trainControl) 
# CART 
set.seed(7) 
fit.cart <- train(Sale.Made~., data=df.mydata, method="rpart", metric=metric, trControl=trainControl) 
# KNN 
set.seed(7) 
fit.knn <- train(Sale.Made~., data=df.mydata, method="knn", metric=metric, trControl=trainControl) 
# SVM 
set.seed(7) 
fit.svm <- train(Sale.Made~., data=df.mydata, method="svmRadial", metric=metric, trControl=trainControl) 
# Random Forest 
set.seed(7) 
fit.rf <- train(Sale.Made~., data=df.mydata, method="rf", metric=metric, trControl=trainControl)
