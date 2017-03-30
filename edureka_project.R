library(dplyr)
library(tidyr)
library(ggplot2)
df.retail <- read.csv("Retail_Case_Study_Data.csv")
summary(df.retail)
str(df.retail)
hist(df.retail$Spend.Numeric, breaks = 50)
ggplot(data=df.retail, aes(x=Spend.Numeric)) +
  geom_histogram(bins = 10)
