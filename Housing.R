#experiment number 3
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

#experiment number 4
#Multiple Linear Regression

library(tidyverse)
dataset <- read.csv('Housing.csv')

# Remove entries with "yes" in the "prefarea" column
new2_dataset <- dataset %>% filter(prefarea == "no")
new_dataset <- select(dataset, -prefarea)
# exploration of new data set
view(new_dataset)
head(new_dataset)
glimpse(new_dataset)
length(new_dataset)
names(new_dataset)
summary(new_dataset)

# missing value
colSums(is.na(new_dataset))
#-- there are no missing values

# multiple linear regression / splitting data into 2 sets
set.seed(100)
split <- sample.split(new_dataset$price, SplitRatio = 0.8) #80% training, 20% test set
Training_Set <- subset(new_dataset, split = TRUE)
Test_Set <- subset(new_dataset, split = FALSE) 

# Multiple linear regression training
names(new_dataset)
Training_Set <- droplevels(Training_Set)
MLR <- lm(formula = price ~ ., data = Training_Set)
summary(MLR)

# mean square error
summ <- summary(MLR)
MSE <- mean(summ$residuals^2)
paste("Mean squared error", MSE)

# R square
summary(MLR)
Test_Set

# testing set prediction
y_pred <- predict(MLR, newdata = Test_Set)
data <- data.frame(Test_Set$price, y_pred)
head(data)








