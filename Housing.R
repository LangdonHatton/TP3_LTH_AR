#Multiple Linear Regression
library(tidyverse)
dataset=read.csv('Housing.csv')

#exploration of data set
view(dataset)
head(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#missing value
colSums(is.na(dataset))
#-- there are no missing values

#multiple linear regression / splitting data into 2 sets
library(caTools)
set.seed(100)
split=sample.split(dataset$price, SplitRatio=0.8) #80% training, 20% test set
Training_Set=subset(dataset,split=TRUE)
Test_Set=subset(dataset,split=FALSE) 

#Multiple linear regression training
names(dataset)
MLR=lm(formula=price~ .,
       data=Training_Set)
summary(MLR)


#mean square error
summ=summary(MLR)
MSE=(mean(summ$residuals^2))
paste("Mean squared error", MSE)

#R square
summary(MLR)
Test_Set

#testing set prediction
y_pred=predict(MLR,newdata=Test_Set)
data=data.frame(Test_Set$price,y_pred)
head(data)

#validation

