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
#convert spend category to factor
df.retail$Spend.Category<-as.factor(df.retail$Spend.Category)
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
# LG 
set.seed(7) 
fit.glm <- train(Sale.Made~., data=df.mydata, method="glm", metric=metric,preProc=c("center", "scale"), trControl=trainControl) 
# GLMNET
set.seed(7) 
fit.glmnet <- train(Sale.Made~., data=df.mydata, method="glmnet", metric=metric,preProc=c("center", "scale"),  trControl=trainControl) 
# LDA 
set.seed(7) 
fit.lda <- train(Sale.Made~., data=df.mydata, method="lda", metric=metric,preProc=c("center", "scale"),  trControl=trainControl) 
# CART 
set.seed(7) 
fit.cart <- train(Sale.Made~., data=df.mydata, method="rpart", metric=metric,preProc=c("center", "scale"),  trControl=trainControl) 
# KNN 
set.seed(7) 
fit.knn <- train(Sale.Made~., data=df.mydata, method="knn", metric=metric,preProc=c("center", "scale"),  trControl=trainControl) 
# NB 
set.seed(7) 
fit.nb <- train(Sale.Made~., data=df.mydata, method="nb", metric=metric, trControl=trainControl) 
# CART 
set.seed(7) 
fit.cart <- train(Sale.Made~., data=df.mydata, method="rpart", metric=metric, trControl=trainControl)
# Compare algorithms 
results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn, CART=fit.cart, NB=fit.nb)) 
summary(results) 
pred <- predict(fit.glm,newdata = validation[-9],type ="response")
summary(fit.glm)
head( validation[-9])
